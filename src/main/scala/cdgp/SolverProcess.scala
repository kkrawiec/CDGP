package cdgp

import java.io._
import java.util.Scanner
import scala.sys.process._
import fuel.util.{Collector, FApp, Options}
import scala.collection.mutable
import scala.util.Random



class UnknownSolverOutputException(message: String = "", cause: Throwable = null)
      extends Exception(message, cause)


object SolverSMT {
  val LOG_FILE: String = "solver_log.txt"

  /**
    * Adds a newline before the first occurence of a left parenthesis.
    * Sometimes Z3 may return something like this:
    *   "unsat(error "line 23 column 11: model is not available")"
    * Normalization introduces \n before the first parenthesis.
    */
  def normalizeOutput(output: String): String = {
    val firstPar = output.indexOf("(")
    if (firstPar == -1)
      output
    else
      output.take(firstPar) + "\n" + output.drop(firstPar)
  }
}

trait SolverSMT extends Closeable {
  /**
    * Executes the query and preprocesses the result, returning decision and optional
    * other content (e.g. model).
    */
  def solve(query: Query): (String, Option[String])
  /**
    * Executes the query and returns raw output of the solver.
    */
  def executeQuery(query: Query): String
  def executeQuery(query: String, satCmds: String = "", unsatCmds: String = ""): String = {
    executeQuery(CheckSatQuery(query, satCmds, unsatCmds))
  }

  protected lazy val logFile = new File(SolverSMT.LOG_FILE)
  protected lazy val logFilePrinter = new PrintWriter(logFile)

  def log(s: String): Unit = {
    logFilePrinter.print(s)
  }

  def close(): Unit = {}
}


/**
  * Saves each query on disk as a temporary file and then executes solver binaries for this query.
  */
case class SolverFromScript(path: String, args: String = SolverFromScript.ARGS_Z3, verbose: Boolean = false,
                            logAllQueries: Boolean = false, seed: Int = 0)
            extends SolverSMT {
  private val rng: Random = scala.util.Random
  rng.setSeed(seed)  // rng is needed to avoid tmp file clashes in case many different processes are run

  def apply(input: String): String = {
    val r = System.currentTimeMillis() + rng.nextInt(1000000000)
    val tmpfile = new File(s"smtlib$r.smt2")
    save(tmpfile, input)
    val stdout = new StringBuilder
    val stderr = new StringBuilder
    val pl = ProcessLogger(stdout append _, stderr append _)
    val cmd = s"$path $args ${tmpfile.getAbsolutePath}"
    try {
      val status = cmd ! pl
    } catch {
      case e: RuntimeException =>
        tmpfile.delete
        throw new Exception(s"Solver failed for input:\n$input\nwith output:\n$stdout\n----- stderr -----\n$stderr\n", e)
    }
    tmpfile.delete
    val out = stdout.toString().trim
    val err = stderr.toString().trim
    if (!out.startsWith("(sat") && !out.startsWith("(unsat") &&
        !out.startsWith("(unknown") && !out.startsWith("(timeout") &&
        err != "")
      throw new Exception(s"Solver encountered an error: $err")
    out
  }

  override def solve(query: Query): (String, Option[String]) = {
    val inputStr = s"${query.getScript}\n"
    val output = SolverSMT.normalizeOutput(executeQuery(inputStr))
    val lines = output.split("\n").map(_.trim)
    val outputDec = lines.head
    val outputRest = if (lines.size == 1) None else Some(lines.tail.mkString("\n"))
    if (outputDec == "sat" || outputDec == "unsat" || outputDec == "unknown" || outputDec == "timeout")
      (outputDec, outputRest)
    else throw new UnknownSolverOutputException(s"Solver did not return sat, unsat, nor unknown, but this: $output.\nQuery:\n$query")
  }

  /** Executes a query and returns raw output as a String. */
  def executeQuery(query: Query): String = executeQuery(query.getScript)

  /** Executes a query and returns raw output as a String. */
  private def executeQuery(inputStr: String): String = {
    if (verbose) println(s"Input to the solver:\n$inputStr\n")
    val output = apply(inputStr).trim
    if (verbose) print("Solver output:\n" + output)
    output
  }

  def save(file: File, s: String): Unit = {
    if (logAllQueries)
      log(s)
    val pw = new PrintWriter(file)
    pw.print(s)
    pw.close()
  }
}

object SolverFromScript {
  // pp.min-alias-size=1000000 pp.max_depth=1000000 are needed for simplification to not have let expressions
  def ARGS_Z3: String = "-smt2 pp.min-alias-size=1000000 pp.max_depth=1000000 " //-file:
  def ARGS_CVC4: String = "--lang=smt2.5 --strings-exp --default-dag-thresh=0 "
  def ARGS_DREAL3: String = "--model "
  def ARGS_OTHER: String = ""
}



/**
  * Executes solver binaries one and works in the interactive mode.
  */
case class SolverInteractive(path: String, args: String = SolverInteractive.ARGS_Z3,
                             verbose: Boolean = false, logAllQueries: Boolean = false)
  extends SolverSMT {

  private[this] var is: OutputStream = _
  private[this] var os: InputStream = _
  private[this] var es: InputStream = _

  private def startProcess: Process = {
    val pb = Process(s"$path $args")
    val pio = new ProcessIO(this.is = _, this.os = _, this.es = _)
    val process = pb.run(pio) // don't wait

    // This sleep magically solves problems with 'is or 'os being randomly
    // null shortly after the creation of the process.
    Thread.sleep(10)
    process
  }
  private[this] val process = startProcess
  private[this] val bis = new PrintWriter(is)
  private[this] val scanner = new Scanner(os)

  private[this] def apply(input: String): Unit = {
    if (logAllQueries)
      log(input)
    bis.println(input)
    bis.flush()
  }

  private[this] def scanOutputInParentheses: String = {
    val sb = new StringBuilder
    var n: Int = 0
    var qMarkOpened: Boolean = false
    do {
      val s = scanner.nextLine
      sb ++= s
      // We must be careful here. One of the models for String problem was:
      // ((s "&(A\x02\x02 \x00")). This '(' in String made the previous version
      // of this code wait infinitely long for new input.
      // NOTE: the code below is still not prepared for the case when '"' char is
      // in the string.
      for (c <- s) c match {
        case '\"' if !qMarkOpened => qMarkOpened = true
        case '\"' if  qMarkOpened => qMarkOpened = false
        case '(' if !qMarkOpened => n = n + 1
        case ')' if !qMarkOpened => n = n - 1
        case _   =>
      }
    } while (n > 0)
    sb.toString
  }

  /*
  private[this] def scanWholeOutput: String = {
    // This method blocks forever...
    val sb = new StringBuilder
    do {
      val s = scanner.nextLine
      sb ++= s
    } while (scanner.hasNextLine)
    sb.toString
  }
  */

  def solve(query: Query): (String, Option[String]) = {
    this.synchronized {
      apply("(reset)\n")  // remove all earlier definitions and constraints
      if (verbose) println(s"Input to the solver:\n$query\n")
      val script = s"${query.code}\n${query.mainCmd}\n"
      apply(script)
      val output = scanner.nextLine
      if (verbose) print(s"Solver output:\n$output\n")
      if (es.available() > 0)
        throw new Exception(s"Solver produced this on stderr: " + new Scanner(es).nextLine)

      def secondQuery(cmds: String): Option[String] = {
        if (cmds.isEmpty) None
        else Some({ apply(query.satCmds); scanOutputInParentheses })
      }

      if (output == "sat") {
        val outputData = secondQuery(query.satCmds)
        ("sat", outputData)
      }
      else if (output == "unsat") {
        val outputData = secondQuery(query.unsatCmds)
        ("unsat", outputData)
      }
      else if (output == "unknown" || output == "timeout") (output, None)
      else throw new UnknownSolverOutputException(s"Solver did not return sat, unsat, nor unknown, but this: $output.\nQuery:\n$query")
    }
  }

  /** Executes a query and returns raw output as a String. */
  def executeQuery(query: Query): String = executeQuery(query.getScript)

  /** Simply executes a query and returns raw output. */
  def executeQuery(inputStr: String): String = {
    if (verbose) println(s"Input to the solver:\n$inputStr\n")
    apply("(reset)\n")  // remove all earlier definitions and constraints
    apply(inputStr)
    val output = scanOutputInParentheses
    if (verbose) print("Solver output:\n" + output)
    output.trim
  }

  override def close(): Unit = {
    is.close()
    os.close()
    es.close()
    process.destroy
  }
}

object SolverInteractive {
  // pp.min-alias-size=1000000 pp.max_depth=1000000 are needed for simplification to not have let expressions
  def ARGS_Z3: String = "-smt2 pp.min-alias-size=1000000 pp.max_depth=1000000 -in "
  // To use timeout with CVC4: --moreSolverArgs "--tlimit-per 1000"
  def ARGS_CVC4: String = "--lang=smt2.5 --strings-exp --default-dag-thresh=0 --incremental "
  def ARGS_OTHER: String = ""
}







object TestSolverOpenConnection extends FApp {
  val n = opt('n, 100)
  val solverPath = opt('solverPath)
  var sum = 0
  0.until(n).foreach { _=>
    try {
      val s = new SolverManager(solverPath)
      sum += s.getNumRestarts
      s.close()
    }
    catch { case e: Throwable => println(s"Exception catched! Msg: ${e.getMessage()}"); sum += 5 }
  }
  println(s"Overall restarts: $sum")
}




class SolverManager(val path: String, val args: Option[String] = None, val moreArgs: String = "",
                    verbose: Boolean = false)
                   (implicit opt: Options, coll: Collector) {
  private val maxSolverRestarts: Int = opt('maxSolverRestarts, 1)
  private val solverInteractive: Boolean = opt('solverInteractive, true)
  private val logAllQueries: Boolean = opt('logAllQueries, false)
  private val solverType: String = opt('solverType, "z3")
  assert(solverType == "z3" || solverType == "cvc4" || solverType == "other",
    "Invalid solver type! --solverType argument accepts values: 'z3', 'cvc4', 'other'.")
  private var doneRestarts: Int = 0
  private val solveTimes: mutable.Map[Double, Int] = mutable.Map[Double, Int]()
  private var numCalls: Int = 0
  private var minSolveTime: Double = 0.0
  private var maxSolveTime: Double = 0.0
  private var sumSolveTime: Double = 0.0
  def getNumRestarts: Int = doneRestarts
  def getNumCalls: Int = numCalls
  def setNumCalls(nc: Int) { numCalls = nc}
  def getSolveTimesAsCountMap: Map[Double, Int] = solveTimes.toMap
  def getSumSolveTime: Double = sumSolveTime
  def getMinSolveTime: Double = solveTimes.keys.min
  def getMaxSolveTime: Double = solveTimes.keys.max
  def getAvgSolveTime: Double = sumSolveTime / solveTimes.values.sum

  private def updateRunStats(timeDiffInSecs: Double): Unit = {
    numCalls += 1
    if (solveTimes.contains(timeDiffInSecs))
      solveTimes.put(timeDiffInSecs, solveTimes(timeDiffInSecs) + 1)
    else
      solveTimes.put(timeDiffInSecs, 1)
    sumSolveTime += timeDiffInSecs
  }

  private var _solver: SolverSMT = createWithRetries()
  def solver: SolverSMT = _solver

  protected def getSolverArgs: String = {
    val ma = if (moreArgs == "") "" else s" $moreArgs "
    if (args.isDefined) ma + args.get
    else if (solverInteractive) {
      ma + (if (solverType == "z3") SolverInteractive.ARGS_Z3
      else if (solverType == "cvc4") SolverInteractive.ARGS_CVC4
      else SolverInteractive.ARGS_OTHER)
    }
    else {
      ma + (if (solverType == "z3") SolverFromScript.ARGS_Z3
      else if (solverType == "cvc4") SolverFromScript.ARGS_CVC4
      else SolverFromScript.ARGS_OTHER)
    }
  }

  /**
    * Sometimes during opening connection with a solver an unidentified error occurs.
    * This function retries opening connection if this happens.
    */
  protected def createWithRetries(): SolverSMT = {
    coll.set("doneSolverRestarts", doneRestarts)
    try {
      val solverArgs = getSolverArgs
      if (solverInteractive)
        SolverInteractive(path, solverArgs, verbose = verbose, logAllQueries = logAllQueries)
      else
        SolverFromScript(path, solverArgs, verbose = verbose, logAllQueries = logAllQueries, seed = opt("seed", 0))
    }
    catch {
      case error: Throwable =>
        if (doneRestarts < maxSolverRestarts) {
          doneRestarts += 1
          println(s"Restarting solver (retry no. $doneRestarts)")
          createWithRetries()
        }
        else throwExceededMaxRestartsException("", error)
    }
  }

  /**
    * Executes provided commands using the SMT solver.
    * @param query Commands to be executed.
    */
  def runSolver(query: Query): (String, Option[String]) = {
    try {
      val start = System.currentTimeMillis()
      val res = solver.solve(query)
      updateRunStats((System.currentTimeMillis() - start) / 1000.0)
      res
    }
    catch {
      // case e : UnknownSolverOutputException => throw e  // we want to fail if any error happens
      // Sometimes error happens when solver runs too long in the interactive mode.
      case e: Throwable => { // Restarting solver, because most likely it crashed.
        if (doneRestarts < maxSolverRestarts) {
          doneRestarts += 1
          _solver = createWithRetries()
          runSolver(query)
        }
        else throwExceededMaxRestartsException(query.toString, e)
      }
    }
  }

  /**
    * Executes the provided query using the SMT solver. Solver's output is returned without any processing.
    * @param query A full query for the solver. Nothing will be added to it except from
    *              the "(reset)" in the beginning in case of interactive solver.
    */
  def executeQuery(query: Query): String = {
    try {
      val start = System.currentTimeMillis()
      val res = solver.executeQuery(query)
      updateRunStats((System.currentTimeMillis() - start) / 1000.0)
      res
    }
    catch {
      //case e : UnknownSolverOutputException => throw e  // we want to fail if any UNKNOWN happens
      // Sometimes error happens when solver runs too long in the interactive mode.
      case e: Throwable => { // Restarting solver, because most likely it crashed.
        if (doneRestarts < maxSolverRestarts) {
          doneRestarts += 1
          _solver = createWithRetries()
          executeQuery(query)
        }
        else throwExceededMaxRestartsException(query.toString, e)
      }
    }
  }


  /**
    * Closes the connection to the solver.
    */
  def close(): Unit = {
    solver.close()
  }

  protected def throwExceededMaxRestartsException(query: String, error: Throwable): Nothing = {
    error.printStackTrace()
    val msg = s"Exceeded the maximum number of $maxSolverRestarts solver restarts. " +
              s"Original message: ${error.getMessage}\nQuery:\n$query"
    coll.set("solverError", error.getMessage)
    coll.set("solverError2", msg)
    coll.saveSnapshot("error_solver")
    throw new ExceededMaxRestartsException(msg)
  }
  class ExceededMaxRestartsException(msg: String) extends RuntimeException(msg) {}
}


object SolverManager {
  def apply(implicit opt: Options, coll: Collector): SolverManager = {
    new SolverManager(opt('solverPath), opt.getOption("solverArgs"), moreArgs=opt('moreArgs, ""),
      verbose=opt('verbose, false))
  }
}
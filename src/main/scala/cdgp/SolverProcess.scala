package cdgp

import java.io._
import java.util.Scanner

import scala.sys.process._
import fuel.util.{Collector, FApp, Options}

import scala.collection.mutable
import scala.concurrent.duration.{Duration, MILLISECONDS}
import scala.concurrent.{Await, Future}
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
  def solve(query: String, satCmds: String = "", unsatCmds: String = ""): (String, Option[String]) = {
    solve(CheckSatQuery(query, satCmds, unsatCmds))
  }
  /**
    * Executes the query and returns raw output of the solver. Commands dependent on the query
    * outcome are not employed.
    */
  def solveRawOutput(query: Query): String
  def solveRawOutput(query: String): String = {
    solveRawOutput(CheckSatQuery(query, "", ""))
  }

  protected lazy val logFile = new File(SolverSMT.LOG_FILE)
  protected lazy val logFilePrinter = new PrintWriter(logFile)

  def log(s: String): Unit = {
    logFilePrinter.print(s)
  }

  def close(): Unit
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
    val output = SolverSMT.normalizeOutput(solveRawOutput(inputStr))
    val lines = output.split("\n").map(_.trim)
    val outputDec = lines.head
    val outputRest = if (lines.size == 1) None else Some(lines.tail.mkString("\n"))
    if (outputDec == "sat" || outputDec == "unsat" || outputDec == "unknown" || outputDec == "timeout")
      (outputDec, outputRest)
    else throw new UnknownSolverOutputException(s"Solver did not return sat, unsat, nor unknown, but this: $output\nQuery:\n$query")
  }

  /** Executes a query and returns raw output as a String. */
  def solveRawOutput(query: Query): String = {
    val inputStr = query.getScript
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

  def close(): Unit = {}
}

object SolverFromScript {
  // pp.min-alias-size=1000000 pp.max_depth=1000000 are needed for simplification to not have let expressions
  def ARGS_Z3: String = "-smt2 pp.decimal_precision=50 pp.single_line=true pp.decimal=true pp.min-alias-size=1000000 pp.max_depth=1000000 " //-file:
  def ARGS_CVC4: String = "--lang=smt2.5 --strings-exp --dag-thresh=0 "
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
    Thread.sleep(20)
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
      // of this code wait infinitely long for a new input.
      // NOTE: it is assumed that solver prints every '"' as '""'.
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
      else throw new UnknownSolverOutputException(s"Solver did not return sat, unsat, nor unknown, but this: $output\nQuery:\n$query")
    }
  }

  /** Executes a query and returns raw output as a String. */
  def solveRawOutput(query: Query): String = {
    val inputStr = query.getScript
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
  // model_validate=true
  def ARGS_Z3: String = "-smt2 pp.single_line=true pp.decimal_precision=50 pp.decimal=true pp.min-alias-size=1000000 pp.max_depth=1000000 -in "
  // To use timeout with CVC4: --moreSolverArgs "--tlimit-per 1000"
  def ARGS_CVC4: String = "--lang=smt2.5 --strings-exp  --dag-thresh=0 --incremental "
  def ARGS_OTHER: String = ""
}







object TestSolverOpenConnection extends FApp {
  val n = 100
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



case class ExceededMaxRestartsException(msg: String) extends RuntimeException(msg) {}


class SolverManager(val path: String, val args: Option[String] = None, val moreArgs: String = "",
                    verbose: Boolean = false)
                   (implicit opt: Options, coll: Collector) {
  private val solverHardTimeout: Option[Int] = opt.getOptionInt("solverHardTimeout")
  private val maxSolverRestarts: Int = opt('maxSolverRestarts, 1)
  private val solverInteractive: Boolean = opt('solverInteractive, true)
  private val logAllQueries: Boolean = opt('logAllQueries, false)
  private val solverType: String = opt('solverType, "z3")
  assert(solverType == "z3" || solverType == "cvc4" || solverType == "other",
    "Invalid solver type! --solverType argument accepts values: 'z3', 'cvc4', 'other'.")
  private var doneRestarts: Int = 0
  private val solveTimes: mutable.Map[Double, Int] = mutable.Map[Double, Int]()
  private var numCalls: Int = 0
  private var sumSolveTime: Double = 0.0
  def getNumRestarts: Int = doneRestarts
  def getNumCalls: Int = numCalls
  def setNumCalls(nc: Int) { numCalls = nc }
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

  private var _solver: SolverSMT = _
  open()  // opens a connection by assigning a created solver object to the _solver
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

  def runWithTimeout[T](timeoutMs: Long)(f: => T) : Option[T] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    Some(Await.result(Future(f), Duration(timeoutMs, MILLISECONDS)))
  }

  /**
    * Executes provided commands using the SMT solver.
    * @param query Commands to be executed.
    */
  def executeQuery(query: Query): (String, Option[String]) = {
    try {
      val start = System.currentTimeMillis()
      val res: (String, Option[String]) = if (solverHardTimeout.isDefined) {
          val x = runWithTimeout(solverHardTimeout.get) {solver.solve(query)}
          if (x.isDefined) x.get else ("timeout", None)
        }
        else
          solver.solve(query)

      updateRunStats((System.currentTimeMillis() - start) / 1000.0)
      res
    }
    catch {
      case e: java.util.concurrent.TimeoutException =>
         // Subtle assumption made here: solverHardTimeout should be different than global timeout
        val text = s"Futures timed out after [${solverHardTimeout.get} milliseconds]"
        if (e.getMessage.contains(text)) {
          close()
          open()
          ("timeout", None)
        }
        else {
          throw e // this was a global timout, throw it once again
        }
      // case e : UnknownSolverOutputException => throw e  // we want to fail if any error happens
      // Sometimes error happens when solver runs too long in the interactive mode.
      case e: Throwable => { // Restarting solver, because most likely it crashed.
        println("Restarting solver due to error")
        if (doneRestarts < maxSolverRestarts) {
          doneRestarts += 1
          close()
          open()
          executeQuery(query)
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
  def executeQueryRawOutput(query: Query): String = {
    try {
      val start = System.currentTimeMillis()
      val res = solver.solveRawOutput(query)
      updateRunStats((System.currentTimeMillis() - start) / 1000.0)
      res
    }
    catch {
      //case e : UnknownSolverOutputException => throw e  // we want to fail if any UNKNOWN happens
      // Sometimes error happens when solver runs too long in the interactive mode.
      case e: Throwable => { // Restarting solver, because most likely it crashed.
        if (doneRestarts < maxSolverRestarts) {
          doneRestarts += 1
          open()
          executeQueryRawOutput(query)
        }
        else throwExceededMaxRestartsException(query.toString, e)
      }
    }
  }



  /**
   * Opens the connection to the solver.
   */
  def open(): Unit = {
    _solver = createWithRetries()
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
              s"Original message: ${error.getMessage}\n" +
              s"Query:\n$query\n"
    throw ExceededMaxRestartsException(msg)
  }


  /**
    * Saves solver-related info and statistics in the collector.
    */
  def reportData(coll: Collector, prefix: String = "solver") {
    coll.set(s"$prefix.totalCalls", getNumCalls)
    coll.set(s"$prefix.totalRestarts", getNumRestarts)
    coll.set(s"$prefix.timeMinSec", getMinSolveTime)
    coll.set(s"$prefix.timeMaxSec", getMaxSolveTime)
    coll.set(s"$prefix.timeAvgSec", getAvgSolveTime)
    coll.set(s"$prefix.timeSumSec", getSumSolveTime)
    coll.set(s"$prefix.allTimesCountMap", getSolveTimesAsCountMap.toList.sortBy(_._1).mkString(", "))
  }
}


object SolverManager {
  def apply(implicit opt: Options, coll: Collector): SolverManager = {
    new SolverManager(opt('solverPath), opt.getOption("solverArgs"), moreArgs=opt('moreArgs, ""),
      verbose=opt('verbose, false))
  }
}
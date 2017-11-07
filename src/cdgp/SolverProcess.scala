package cdgp

import java.io._
import java.util.Scanner
import scala.sys.process._
import fuel.util.{Collector, FApp, Options}
import scala.collection.mutable
import scala.util.Random



class UnknownSolverOutputException(message: String = "", cause: Throwable = null)
      extends Exception(message, cause)

trait SolverSMT extends Closeable {
  /**
    * Executes the query and preprocesses the result, returning decision and optional
    * other content (e.g. model).
    */
  def solve(query: String, postCommands: String*): (String, Option[String])
  /**
    * Executes the query and returns raw output of the solver.
    */
  def executeQuery(query: String): String
}


/**
  * Saves each query on disk as a temporary file and then executes solver binaries for this query.
  */
case class SolverFromScript(path: String, args: String = SolverFromScript.ARGS_Z3, verbose: Boolean = false,
                            seed: Int = 0)
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
    stdout.toString()
  }

  def normalizeOutput(output: String): String = {
    val firstPar = output.indexOf("(")
    if (firstPar == -1)
      output
    else
      output.take(firstPar) + "\n" + output.drop(firstPar)
  }

  override def solve(input: String, postCommands: String*): (String, Option[String]) = {
    val inputStr = s"$input\n${postCommands.mkString}"
    val output = normalizeOutput(executeQuery(inputStr))
    // Sometimes Z3 may return something like this:
    //   "unsat(error "line 23 column 11: model is not available")"
    // Normalization introduces \n before the first parenthesis.
    val lines = output.split("\n").map(_.trim)
    val outputDec = lines.head
    val outputRest = if (lines.size == 1) None else Some(lines.tail.mkString("\n"))
    if (outputDec == "sat" || outputDec == "unsat" || outputDec == "unknown" || outputDec == "timeout")
      (outputDec, outputRest)
    else throw new Exception(s"Solver did not return sat, unsat, nor unknown, but this: $output")
  }

  /** Executes a query and returns raw output as a String. */
  def executeQuery(inputStr: String): String = {
    if (verbose) println(s"Input to the solver:\n$inputStr\n")
    val output = apply(inputStr).trim
    if (verbose) print("Solver output:\n" + output)
    output
  }

  def save(file: File, s: String): Unit = {
    val pw = new PrintWriter(file)
    pw.print(s)
    pw.close()
  }
  override def close(): Unit = {}
}

object SolverFromScript {
  // pp.min-alias-size=1000000 pp.max_depth=1000000 are needed for simplification to not have let expressions
  def ARGS_Z3: String = "-smt2 pp.min-alias-size=1000000 pp.max_depth=1000000 " //-file:
  def ARGS_CVC4: String = "--lang=smt2.5 --default-dag-thresh=0 "
  def ARGS_OTHER: String = ""
}



/**
  * Executes solver binaries one and works in the interactive mode.
  */
case class SolverInteractive(path: String, args: String = SolverInteractive.ARGS_Z3, verbose: Boolean = false)
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
    bis.println(input)
    bis.flush()
  }

  private[this] def scanOutputInParentheses: String = {
    val sb = new StringBuilder
    var n: Int = 0
    do {
      val s = scanner.nextLine
      sb ++= s
      for (c <- s) c match {
        case '(' => n = n + 1
        case ')' => n = n - 1
        case _   =>
      }
    } while (n > 0)
    sb.toString
  }

  private[this] def scanWholeOutput: String = {
    val sb = new StringBuilder
    do {
      val s = scanner.nextLine
      sb ++= s
    } while (scanner.hasNextLine)
    sb.toString
//    var line: String = ""
//    while (!((line = scanner.nextLine()).isEmpty())) {
//      val s = scanner.nextLine
//      sb ++= s
//    }
//    sb.toString
  }

  def solve(input: String, postCommands: String*): (String, Option[String]) = {
    this.synchronized {
      bis.println("(reset)")  // remove all earlier definitions and constraints
      if (verbose) println(s"Input to the solver:\n$input\n")
      apply(input) // TODO: postCommands are ignored!
      val output = scanner.nextLine
      if (verbose) print(s"Solver output:\n$output\n")
      if (es.available() > 0)
        throw new Exception(s"Solver produced this on stderr: " + (new Scanner(es)).nextLine)
      if (output == "sat") {
        val outputData = Some(if (postCommands.isEmpty) "" else {
          apply(postCommands.mkString)
          scanOutputInParentheses
        })
        ("sat", outputData)
      }
      else if (output == "unsat" || output == "unknown" || output == "timeout") (output, None)
      else throw new UnknownSolverOutputException(s"Solver did not return sat, unsat, nor unknown, but this: $output")
    }
  }

  /** Simply executes a query and returns raw output. */
  def executeQuery(inputStr: String): String = {
    if (verbose) println(s"Input to the solver:\n$inputStr\n")
    bis.println("(reset)")  // remove all earlier definitions and constraints
    apply(inputStr)
    val output = scanWholeOutput
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
  def ARGS_CVC4: String = "--lang=smt2.5 --default-dag-thresh=0 --incremental "
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
        SolverInteractive(path, solverArgs, verbose = verbose)
      else
        SolverFromScript(path, solverArgs, verbose = verbose, seed = opt("seed", 0))
    }
    catch {
      case error: Throwable =>
        if (doneRestarts < maxSolverRestarts) {
          doneRestarts += 1
          println(s"Restarting solver (retry no. $doneRestarts)")
          createWithRetries()
        }
        else throwExceededMaxRestartsException(error)
    }
  }

  /**
    * Executes provided commands using the SMT solver.
    * @param query Commands to be executed. (check-sat) is expected to be a part of the query.
    * @param postCommands Additional commands to be placed after the query.
    * @return Solver's decision ('sat', 'unsat', 'unknown', 'timeout') and optional content determined
    *         by postCommands.
    */
  def runSolver(query: String, postCommands: String*): (String, Option[String]) = {
    try {
      val start = System.currentTimeMillis()
      val res = solver.solve(query, postCommands:_*)
      updateRunStats((System.currentTimeMillis() - start) / 1000.0)
      res
    }
    catch {
      case e : UnknownSolverOutputException => throw e  // we want to fail if any UNKNOWN happens
      case e: Throwable => { // Restarting solver, because most likely it crashed.
        if (doneRestarts < maxSolverRestarts) {
          doneRestarts += 1
          _solver = createWithRetries()
          runSolver(query, postCommands: _*)
        }
        else throwExceededMaxRestartsException(e)
      }
    }
  }

  /**
    * Executes the provided query using the SMT solver. Output is returned as is.
    * @param query A full query for the solver. Nothing will be added to it except from
    *              the "(reset)" in the beginning in case of interactive solver.
    */
  def executeQuery(query: String): String = {
    try {
      val start = System.currentTimeMillis()
      val res = solver.executeQuery(query)
      updateRunStats((System.currentTimeMillis() - start) / 1000.0)
      res
    }
    catch {
      case e : UnknownSolverOutputException => throw e  // we want to fail if any UNKNOWN happens
      case e: Throwable => { // Restarting solver, because most likely it crashed.
        if (doneRestarts < maxSolverRestarts) {
          doneRestarts += 1
          _solver = createWithRetries()
          executeQuery(query)
        }
        else throwExceededMaxRestartsException(e)
      }
    }
  }

  /**
    * Closes the connection to the solver.
    */
  def close(): Unit = {
    solver.close()
  }

  protected def throwExceededMaxRestartsException(error: Throwable): Nothing = {
    error.printStackTrace()
    val msg = s"Exceeded the maximum number of $maxSolverRestarts solver restarts. " +
              s"Original message: ${error.getMessage}"
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
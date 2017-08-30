package cdgp

import java.io._
import java.util.Scanner

import scala.sys.process._
import fuel.util.{Collector, FApp, Options}

import scala.util.Random



class UnknownSolverOutputException(message: String = "", cause: Throwable = null)
      extends Exception(message, cause)

trait SolverSMT extends Closeable {
  def solve(input: String, postCommands: String*): (String, Option[String])
}


/**
  * Saves each query on disk as a temporary file and then executes solver binaries
  * for this query.
  */
case class SolverFromScript(path: String, args: String = "-smt2 -file:", verbose: Boolean = false,
                            seed: Int = 0)
            extends SolverSMT {
  private val rng: Random = scala.util.Random
  rng.setSeed(seed)  // rng is needed to avoid tmp file clashes in case many different processes are run

  def apply(input: String): String = {
    val r = rng.nextInt(1000000000)
    val tmpfile = File.createTempFile("smtlib"+r, ".tmp")
    save(tmpfile, input)
    var res: String = ""
    try {
      res = f"$path $args${tmpfile.getAbsolutePath}" !! // strangely may require one empty line after
    } catch {
      case e: RuntimeException => throw new Exception(f"Solver failed for input:\n$input\nwith output:\n$res\n", e)
    }
    tmpfile.delete
    res
  }

  override def solve(input: String, postCommands: String*): (String, Option[String]) = {
    val inputStr = f"$input\n(check-sat)\n${postCommands.mkString}"
    if (verbose) println(f"Input to the solver:\n$inputStr\n")
    val output = apply(inputStr).trim
    if (verbose) print("Solver output:\n" + output)
    val lines = output.split("\n").map(_.trim)
    val outputDec = lines.head
    val outputRest = if (lines.size == 1) None else Some(lines.tail.mkString("\n"))
    if (outputDec == "sat" || outputDec == "unsat" || outputDec == "unknown")
      (outputDec, outputRest)
    else throw new Exception(f"Solver did not return sat, unsat, nor unknown, but this: $output")
  }

  def save(file: File, s: String): Unit = {
    val pw = new PrintWriter(file)
    pw.print(s)
    pw.close()
  }
  override def close(): Unit = {}
}

object SolverFromScript {
  def ARGS_Z3: String = "-smt2 -file:"
  def ARGS_CVC4: String = "--lang=smt2.5 "
}



/**
  * Executes solver binaries one and works in the interactive mode.
  */
case class SolverInteractive(path: String, args: String = "-in", verbose: Boolean = false)
            extends SolverSMT {

  private[this] var is: OutputStream = _
  private[this] var os: InputStream = _
  private[this] var es: InputStream = _

  private def startProcess: Process = {
    val pb = Process(f"$path $args")
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

  private[this] def scanOutputInParentheses = {
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

  def solve(input: String, postCommands: String*): (String, Option[String]) = {
    this.synchronized {
      bis.println("(reset)")
      val inputStr = f"$input(check-sat)"
      if (verbose) println(f"Input to the solver:\n$inputStr\n")
      apply(inputStr)
      val output = scanner.nextLine
      if (verbose) print(f"Solver output:\n$output\n")
      if (es.available() > 0)
        throw new Exception(f"Solver produced this on stderr: " + (new Scanner(es)).nextLine)
      if (output == "sat") {
        val outputData = Some(if (postCommands.isEmpty) "" else {
          apply(postCommands.mkString)
          scanOutputInParentheses
        })
        ("sat", outputData)
      }
      else if (output == "unsat" || output == "unknown") (output, None)
      else throw new UnknownSolverOutputException(f"Solver did not return sat, unsat, nor unknown, but this: $output")
    }
  }

  override def close(): Unit = {
    is.close()
    os.close()
    es.close()
    process.destroy
  }
}

object SolverInteractive {
  def ARGS_Z3: String = "-smt2 -in "
  def ARGS_CVC4: String = "--lang=smt2.5 --incremental "
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
    catch { case e: Throwable => println(f"Exception catched! Msg: ${e.getMessage()}"); sum += 5 }
  }
  println(s"Overall restarts: $sum")
}




class SolverManager(path: String, args: Option[String] = None, verbose: Boolean = false)
                   (implicit opt: Options, coll: Collector) {
  private val maxSolverRestarts: Int = opt('maxSolverRestarts, 5)
  private val solverInteractive: Boolean = opt('solverInteractive, true)
  private val solverType: String = opt('solverType, "z3")
  assert(solverType == "z3" || solverType == "cvc4", "Invalid solver type! --solverType argument accepts values: 'z3', 'cvc4'.")
  private var doneRestarts: Int = 0
  private var numCalls: Int = 0
  def getNumRestarts: Int = doneRestarts
  def getNumCalls: Int = numCalls
  def setNumCalls(nc: Int) { numCalls = nc}

  private var _solver: SolverSMT = createWithRetries()
  def solver: SolverSMT = _solver

  protected def getSolverArgs: String = {
    if (args.isDefined) args.get
    else if (solverInteractive) {
      if (solverType == "z3") SolverInteractive.ARGS_Z3
      else SolverInteractive.ARGS_CVC4
    }
    else {
      if (solverType == "z3") SolverFromScript.ARGS_Z3
      else SolverFromScript.ARGS_CVC4
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
          println(f"Restarting solver (retry no. $doneRestarts)")
          createWithRetries()
        }
        else throwExceededMaxRestartsException(error)
    }
  }

  /**
    * Executes provided commands using the SMT solver.
    * @param cmd Commands to be executed.
    * @param postCommands Additional commands to be placed after (check-sat).
    * @return Solver's decision ('sat', 'unsat', 'unknown') and optional content determined
    *         by postCommands.
    */
  def runSolver(cmd: String, postCommands: String*): (String, Option[String]) = {
    try {
      numCalls += 1
      solver.solve(cmd, postCommands:_*)
    }
    catch {
      case e : UnknownSolverOutputException => throw e  // we want to fail if any UNKNOWN happens
      case e: Throwable => { // Restarting solver, because most likely it crashed.
        if (doneRestarts < maxSolverRestarts) {
          doneRestarts += 1
          _solver = createWithRetries()
          runSolver(cmd, postCommands: _*)
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
    val msg = f"Exceeded the maximum number of $maxSolverRestarts solver restarts. " +
              f"Original message: ${error.getMessage}"
    coll.set("solverError", error.getMessage)
    coll.set("solverError2", msg)
    coll.saveSnapshot("error_solver")
    throw new ExceededMaxRestartsException(msg)
  }
  class ExceededMaxRestartsException(msg: String) extends RuntimeException(msg) {}
}
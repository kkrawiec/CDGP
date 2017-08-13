package sygusgp

import java.io.Closeable
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter
import java.util.Scanner
import fuel.util.{Collector, Options}
import scala.sys.process.Process
import scala.sys.process.ProcessIO


class UnknownSolverOutputException(message: String = "", cause: Throwable = null)
      extends Exception(message, cause)


case class Solver(path: String, args: String = "-in", verbose: Boolean = false)
      extends Closeable {

  private[this] var is: OutputStream = null
  private[this] var os: InputStream = null
  private[this] var es: InputStream = null

  // Use z3's standard input
  private[this] val pb = Process(f"$path $args")
  private[this] val pio = new ProcessIO(this.is = _,
    this.os = _,
    this.es = _)
  private[this] val process = pb.run(pio) // don't wait
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
      else throw new UnknownSolverOutputException(f"Solver did not return sat nor unsat, but this: $output")
    }
  }

  override def close(): Unit = {
    is.close()
    os.close()
    es.close()
    process.destroy
  }
}



class SolverManager(path: String, args: String = "-in", verbose: Boolean = false)
                   (implicit opt: Options, coll: Collector) {
  private val maxSolverRestarts: Int = opt('maxSolverRestarts, 5)
  private var doneRestarts: Int = 0
  private var numCalls: Int = 0
  def getNumCalls: Int = numCalls
  def setNumCalls(nc: Int) { numCalls = nc}

  private var _solver: Solver = createWithRetries()
  def solver: Solver = _solver

  /**
    * Sometimes during opening connection with a solver an unidentified error occurs.
    * This function retries opening connection if this happens.
    */
  protected def createWithRetries(): Solver = {
    coll.set("doneSolverRestarts", doneRestarts)
    try {
      Solver(path, args, verbose=verbose)
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
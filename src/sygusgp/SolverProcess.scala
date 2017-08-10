package sygusgp

import java.io.Closeable
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter
import java.util.Scanner

import scala.sys.process.Process
import scala.sys.process.ProcessIO

case class UnknownSolverOutputException(message: String = "", cause: Throwable = null)
          extends Exception(message, cause)

case class Solver(path: String, verbose: Boolean = false)
    extends Closeable {

  private[this] var is: OutputStream = null
  private[this] var os: InputStream = null
  private[this] var es: InputStream = null

  private[this] var numCalls: Int = 0
  def getNumCalls = numCalls
  def setNumCalls(nc: Int) { numCalls = nc}

  // Use z3's standard input
  private[this] val pb = Process(path + " -in")
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
      numCalls = numCalls + 1
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

  override def close: Unit = {
    is.close
    os.close
    es.close
    process.destroy
  }
}


package sygusgp

import java.io.File
import java.io.PrintWriter
import scala.sys.process._

/* Object responsible for launching the solver 
 * 
 */
case class SolverOld(path: String, verbose: Boolean = false) {
  private var numCalls: Int = 0
  def getNumCalls = numCalls

  def apply(input: String) = {
    // TODO: (not essential) using std input (with 'z3 -in') turned out to be tricky, 
    // so temp file for now
    val tmpfile = File.createTempFile("smtlib", ".tmp")
    save(tmpfile, input)
    var res: String = ""
    try {
      res = f"$path ${tmpfile.getAbsolutePath}" !! // strangely may require one empty line after 
    } catch {
      case e: RuntimeException => //throw new Exception(f"Solver failed for input:\n$input\nwith output:\n$res\n", e)
    }
    tmpfile.delete
    res
  }

  def solve(input: String, postCommands: String*) = {
    numCalls = numCalls + 1
    val inputStr = f"$input(check-sat)${postCommands.mkString}"
    if (verbose) println(f"Input to the solver:\n$inputStr\n")
    val output = apply(inputStr)
    if (verbose) print("Solver output:\n" + output)
    if (output.startsWith("sat")) (true, output.drop(3))
    else if (output.startsWith("unsat")) (false, output.drop(5))
    else throw new Exception(f"Solver did not return sat nor unsat, but this: $output")
  }

  def save(file: File, s: String): Unit = {
    val pw = new PrintWriter(file)
    pw.print(s)
    pw.close()
  }
}


package tests

object Global {
  // This works for a solver binary in the path; if you want to point to a
  // specific file on a disk you can provide here a path to it.
  val solverPath: String = "/home/iwob/Programy/Z3/z3" // "z3"
  val solverType: String = "z3"  // Possible values: z3, cvc4, other
  val solverArgs: Option[String] = None
  val moreSolverArgs: String = ""
  val solverInteractive: String = "true"

  def solverConfig: String = {
    s"--solverPath ${Global.solverPath} " +
    s"--solverType ${Global.solverType} " +
    s"--solverInteractive ${Global.solverInteractive}" +
    (if (solverArgs.isDefined) s"--solverArgs ${Global.solverArgs} " else "") +
    (if (Global.moreSolverArgs != "") s"--moreSolverArgs ${Global.moreSolverArgs} " else "")
  }
}

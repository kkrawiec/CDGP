package tests

object Global {
  // This works for a solver binary in the path; if you want to point to a
  // specific file on a disk you can provide here a path to it.
  val solverPath: String = "z3"
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

  val specFirstname: String =
    """(set-logic SLIA)
      |(synth-fun f ((name String)) String ((Start String (name))))
      |
      |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      |
      |(define-fun ithSplit ((s String) (delimiter String) (i Int)) String
      |    (let ((firstSpacePos Int (str.indexof s delimiter 0)))
      |      (let ((SecondSpacePos Int (str.indexof s delimiter (+ firstSpacePos 1))))
      |            (ite (= i 0)
      |                (ite (= firstSpacePos (- 1))
      |                     s ; Return the whole string, there was no space
      |                     (str.substr s 0 firstSpacePos))
      |                (ite (= i 1)
      |                    (ite (= firstSpacePos (- 1))
      |                        "" ; There was no space, so index 1 is out of bounds
      |                        (ite (= SecondSpacePos (- 1))
      |                            (str.substr s (+ firstSpacePos 1) (str.len s)) ; till the end of the String
      |                            (str.substr s (+ firstSpacePos 1) (- (- SecondSpacePos 1) firstSpacePos)) ; to the next space; second arg of str.substr is shift, not position
      |                        )
      |                    )
      |                    "" ; Unhandled values of i
      |                )
      |            )
      |      )
      |    )
      |)
      |
      |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      |
      |; (constraint (= (f "Nancy FreeHafer") "Nancy"))
      |(declare-var s String)
      |(constraint (=> (and (distinct (str.indexof s " " 0) (- 1))  (>= (str.len s) 3)  (distinct (str.at s 0) " ")  (distinct (str.at s (- (str.len s) 1)) " "))
      |   (= (f s) (ithSplit s " " 0) )))
      |(check-synth)""".stripMargin
}

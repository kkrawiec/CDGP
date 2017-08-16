package sygusgp

import fuel.func.RunExperiment
import fuel.util.FApp
import swim.tree.Op



/**
  * Entry point to run various pre-built variants of CDGP.
  *
  * Obligatory options:
  * --searchAlgorithm, accepted values: GP, GPSteadyState, Lexicase, LexicaseSteadyState
  * --benchmark, path to the SyGuS benchmark
  * --solverPath, path to the SMT solver (e.g. Z3).
  */
object Main extends FApp {
  val cdgpState = CDGPState(opt('benchmark))

  val (res, bestOfRun) = opt('searchAlgorithm) match {
    case "GP" => {
      val alg = CDGPGenerational(opt('benchmark))
      val finalPop = RunExperiment(alg)
      (finalPop, alg.bsf.bestSoFar)
    }
    case "GPSteadyState" => {
      val alg = CDGPSteadyState(opt('benchmark))
      val finalPop = RunExperiment(alg)
      (finalPop, alg.bsf.bestSoFar)
    }
    case "Lexicase" => {
      val alg = CDGPGenerationalLexicase(opt('benchmark))
      val finalPop = RunExperiment(alg)
      (finalPop, alg.bsf.bestSoFar)
    }
    case "LexicaseSteadyState" => {
      val alg = CDGPSteadyStateLexicase(opt('benchmark))
      val finalPop = RunExperiment(alg)
      (finalPop, alg.bsf.bestSoFar)
    }
  }


  def isOptimal(bestOfRun: (Op, Any)): Boolean = {
    bestOfRun._2 match {
      case a if a.isInstanceOf[Int]      => a.asInstanceOf[Int] == -1
      case a if a.isInstanceOf[Seq[Int]] => a.asInstanceOf[Seq[Int]].nonEmpty && a.asInstanceOf[Seq[Int]].head == -1
    }
  }
  val passedTestsRatio = coll.getResult("best.passedTestsRatio").getOrElse("n/a")
  println("\nBest program found: " + coll.getResult("best").getOrElse("n/a"))
  println("Evaluation: " + coll.getResult("best.eval").getOrElse("n/a"))
  println("Ratio of passed tests: " + passedTestsRatio)
  println("Total solver calls: " + cdgpState.solver.getNumCalls)
  println("Total time [ms]: " + coll.getResult("totalTimeSystem").getOrElse("Unknown"))
  //println("Total tests: " + testsManager.tests.size)


  assume(bestOfRun.isDefined, "No solution (optimal or approximate) to the problem was found.")
  val solutionCode = SMTLIBFormatter.synthTaskSolutionToString(cdgpState.synthTask, bestOfRun.get._1)

  println("\nOPTIMAL SOLUTION:")
  if (isOptimal(bestOfRun.get)) println(solutionCode) else println("unknown")

  if (!isOptimal(bestOfRun.get)) {
    println(f"\nAPPROXIMATED SOLUTION:\n(passedTestsRatio $passedTestsRatio)")
    println(solutionCode)
  }
}

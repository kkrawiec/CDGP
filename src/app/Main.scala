package app

import java.io.{PrintWriter, StringWriter}

import fuel.func.RunExperiment
import fuel.util.FApp
import swim.tree.Op
import cdgp._



/**
  * Entry point to run various pre-built variants of CDGP.
  *
  * Obligatory options:
  * --searchAlgorithm, accepted values: GP, GPSteadyState, Lexicase, LexicaseSteadyState
  * --benchmark, path to the SyGuS benchmark
  * --solverPath, path to the SMT solver (e.g. Z3)
  */
object Main extends FApp {
  try {
    val benchmark = opt('benchmark)
    println(s"Benchmark: $benchmark")
    val cdgpState = CDGPState(benchmark)

    val (res, bestOfRun) = opt('searchAlgorithm) match {
      case "GP" => {
        val alg = CDGPGenerational(cdgpState)
        val finalPop = RunExperiment(alg)
        (finalPop, alg.bsf.bestSoFar)
      }
      case "GPSteadyState" => {
        val alg = CDGPSteadyState(cdgpState)
        val finalPop = RunExperiment(alg)
        (finalPop, alg.bsf.bestSoFar)
      }
      case "Lexicase" => {
        val alg = CDGPGenerationalLexicase(cdgpState)
        val finalPop = RunExperiment(alg)
        (finalPop, alg.bsf.bestSoFar)
      }
      case "LexicaseSteadyState" => {
        val alg = CDGPSteadyStateLexicase(cdgpState)
        val finalPop = RunExperiment(alg)
        (finalPop, alg.bsf.bestSoFar)
      }
    }
    coll.saveSnapshot("cdgp")

    /////////////////////// Printing results ///////////////////////////////

    def isOptimal(bestOfRun: (Op, Any)): Boolean = {
      bestOfRun._2 match {
        case a if a.isInstanceOf[FInt] => a.asInstanceOf[FInt].correct
        case a if a.isInstanceOf[FSeqInt] => a.asInstanceOf[FSeqInt].correct
      }
    }

    val passedTestsRatio = coll.getResult("best.passedTestsRatio").getOrElse("n/a")
    println("\n\nBest program found:\t\t" + coll.getResult("best").getOrElse("n/a"))
    println("Evaluation:\t\t\t\t" + coll.getResult("best.eval").getOrElse("n/a"))
    println("Ratio of passed tests:\t" + passedTestsRatio)
    println("Tests total:\t\t\t" + cdgpState.testsManager.getNumberOfTests)
    println("Tests known outputs:\t" + cdgpState.testsManager.getNumberOfKnownOutputs)
    println("Total solver calls:\t\t" + cdgpState.solver.getNumCalls)
    println("Total time [s]:\t\t\t" + coll.getResult("totalTimeSystem").get.toString.toInt / 1000.0)


    assume(bestOfRun.isDefined, "No solution (optimal or approximate) to the problem was found.")
    val solutionCode = SMTLIBFormatter.synthTaskSolutionToString(cdgpState.synthTask, bestOfRun.get._1)

    println("\nOPTIMAL SOLUTION:")
    if (isOptimal(bestOfRun.get))
      println(solutionCode) else println("unknown")

    if (!isOptimal(bestOfRun.get)) {
      println(f"\nAPPROXIMATED SOLUTION:\n(passedTestsRatio $passedTestsRatio)")
      println(solutionCode)
    }
  }
  catch {
    case e: Throwable =>
      println(s"Terminating exception occurred! Message: ${e.getMessage}")
      coll.set("terminatingException", e.toString)
      val sw = new StringWriter
      e.printStackTrace(new PrintWriter(sw))
      coll.set("terminatingExceptionStacktrace", sw.toString)
      coll.saveSnapshot("cdgp.error")
      e.printStackTrace()
  }
}

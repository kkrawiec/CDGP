package app

import java.io.{File, PrintWriter, StringWriter}

import fuel.func.RunExperiment
import fuel.util.{CollectorFile, Options, OptionsMap, Rng}
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
object Main {
  def getOptions(args: Array[String]): Options = {
    val opt = Options(args)
    if (opt.getOption("loadOptionsFromFile").isDefined) {
      println("Options loaded from file: " + opt.paramString("loadOptionsFromFile"))
      val tmp = Options.loadFromFile(new File(opt.getOption("loadOptionsFromFile").get))
      new OptionsMap(tmp.allOptions.filter{ case (k, v) => !k.startsWith("result") })
    }
    else opt
  }

  def main(args: Array[String]): Unit = {
    implicit val opt = getOptions(args)
    implicit val coll = CollectorFile(opt)
    implicit val rng = Rng(opt)
    try {
      val benchmark = opt('benchmark)
      println(s"Benchmark: $benchmark")
      val cdgpState = CDGPState(benchmark)

      val (res, bestOfRun) = opt('searchAlgorithm) match {
        case "GP" =>
          val alg = CDGPGenerational(cdgpState)
          val finalPop = RunExperiment(alg)
          (finalPop, alg.bsf.bestSoFar)

        case "GPSteadyState" =>
          val alg = CDGPSteadyState(cdgpState)
          val finalPop = RunExperiment(alg)
          (finalPop, alg.bsf.bestSoFar)

        case "Lexicase" =>
          val alg = CDGPGenerationalLexicase(cdgpState)
          val finalPop = RunExperiment(alg)
          (finalPop, alg.bsf.bestSoFar)

        case "LexicaseSteadyState" =>
          val alg = CDGPSteadyStateLexicase(cdgpState)
          val finalPop = RunExperiment(alg)
          (finalPop, alg.bsf.bestSoFar)
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
      val pn = 26
      println("\n")
      println("Best program found:".padTo(pn, ' ') + coll.getResult("best").getOrElse("n/a"))
      println("Evaluation:".padTo(pn, ' ') + coll.getResult("best.eval").getOrElse("n/a"))
      println("Program size:".padTo(pn, ' ') + coll.getResult("best.size").get)
      println("Ratio of passed tests:".padTo(pn, ' ') + passedTestsRatio)
      println("Tests total:".padTo(pn, ' ') + cdgpState.testsManager.getNumberOfTests)
      println("Tests known outputs:".padTo(pn, ' ') + cdgpState.testsManager.getNumberOfKnownOutputs)
      println("Total solver calls:".padTo(pn, ' ') + cdgpState.solver.getNumCalls)
      println("Generations:".padTo(pn, ' ') + coll.getResult("best.generation").get)
      println("Total time [s]:".padTo(pn, ' ') + coll.getResult("totalTimeSystem").get.toString.toInt / 1000.0)
      println("Log file:".padTo(pn, ' ') + coll.get("thisFileName").get.toString)


      assume(bestOfRun.isDefined, "No solution (optimal or approximate) to the problem was found.")
      val solutionCode = SMTLIBFormatter.synthSolutionToString(cdgpState.synthTask, bestOfRun.get._1)

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
}

package app

import java.io.{PrintWriter, StringWriter}

import app.Main.{getOptions, printResults, watchTime}
import cdgp._
import fuel.core.StatePop
import fuel.func._
import fuel.util._
import swim.tree.Op

object Regression {

  def runConfigRegressionCDGP(state: StateCDGP, selection: String, evoMode: String)
                             (implicit coll: Collector, opt: Options, rng: TRandom):
  (Option[StatePop[(Op, Fitness)]], Option[(Op, Fitness)]) = {
    (selection, evoMode) match {
      case ("tournament", "generational") =>
        val eval = new EvalCDGPDoubleMSE(state)
        val alg = CDGPGenerational(eval)
        val finalPop = watchTime(alg, RunExperiment(alg))
        (finalPop, alg.bsf.bestSoFar)

      case ("tournament", "steadyState") =>
        ???

      case ("lexicase", "generational") =>
        println("---------  REGRESSION -----------")
        val eval = new EvalCDGPSeqDouble(state)
        val alg = CDGPGenerationalLexicaseR(eval)
        val finalPop = watchTime(alg, RunExperiment(alg))
        (finalPop, alg.bsf.bestSoFar)

      case ("lexicase", "steadyState") =>
        ???
    }
  }


  // --------------------------------------------------------------------------
  //                                 MAIN
  // --------------------------------------------------------------------------

  def main(args: Array[String]): Unit = {
    implicit val opt = getOptions(args)
    implicit val coll = CollectorFile(opt)
    implicit val rng = Rng(opt)

    try {
      val benchmark = opt('benchmark)
      println(s"Benchmark: $benchmark")

      val method = opt('method)
      val selection = opt('selection, "lexicase")
      val evoMode = opt('evolutionMode, "generational")
      assert(method == "CDGP" || method == "GP", s"Invalid method '$method'! Possible values: 'CDGP', 'GP'.")
      assert(selection == "tournament" || selection == "lexicase",
        s"Invalid selection: '$selection'! Possible values: 'tournament', 'lexicase'.")
      assert(evoMode == "generational" || evoMode == "steadyState",
        s"Invalid evolutionMode: '$evoMode'! Possible values: 'generational', 'steadyState'.")

      // Create CDGP state
      val cdgpState = StateCDGP(benchmark)


      // Run algorithm
      val (_, bestOfRun) = runConfigRegressionCDGP(cdgpState, selection, evoMode)


      // Print and save results
      coll.saveSnapshot("cdgp")
      printResults(cdgpState, bestOfRun)
    }
    catch {
      case e: NoSolutionException =>
        println(s"There is no solution to this problem.")
        println(s"Input with no correct answer: " + e.badInput)
        coll.set("cdgp.noCorrectSolution", true)
        coll.set("terminatingException", e.toString)
        coll.saveSnapshot("cdgp")
      case e: java.util.concurrent.TimeoutException =>
        println("Timeout!!!!!!!!!!!!!!!!!!!")
        coll.set("cdgp.wasTimeout", true)
        coll.saveSnapshot("cdgp")
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

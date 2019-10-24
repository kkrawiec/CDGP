package app

import java.io.{PrintWriter, StringWriter}

import cdgp._
import fuel.core.StatePop
import fuel.func._
import fuel.util._
import swim.tree.Op


object RegressionCDGP {

  def getEvalForMSE(state: StateCDGP, method: String)
                   (implicit coll: Collector, opt: Options, rng: TRandom):
  EvalCDGPContinuous[FDouble] = {

    if (method == "CDGPprops") {
      val testsTypesForRatio = opt('testsTypesForRatio, "i,s").split(",").toSet
      new EvalCDGPDoubleMSE(state, testsTypesForRatio)
    }
    else if (method == "CDGP") {
      val testsTypesForRatio = opt('testsTypesForRatio, "i").split(",").toSet
      opt.retrievedOptions
      new EvalCDGPDoubleMSE(state, testsTypesForRatio)
    }
    else {
      val testsTypesForRatio = opt('testsTypesForRatio, "i").split(",").toSet
      new EvalGPDoubleMSE(state, testsTypesForRatio)
    }
  }

  def getEvalForSeqDouble(state: StateCDGP, method: String)
                         (implicit coll: Collector, opt: Options, rng: TRandom):
  EvalCDGPContinuous[FSeqDouble] = {
    if (method == "CDGPprops") {
      val testsTypesForRatio = opt('testsTypesForRatio, "i,s").split(",").toSet
      new EvalCDGPSeqDouble(state, testsTypesForRatio)
    }
    else if (method == "CDGP") {
      val testsTypesForRatio = opt('testsTypesForRatio, "i").split(",").toSet
      new EvalCDGPSeqDouble(state, testsTypesForRatio)
    }
    else {
      val testsTypesForRatio = opt('testsTypesForRatio, "i").split(",").toSet
      new EvalGPSeqDouble(state, testsTypesForRatio)
    }
  }

  def runConfigRegressionCDGP(state: StateCDGP, method: String, selection: String, evoMode: String)
                             (implicit coll: Collector, opt: Options, rng: TRandom):
  ((Option[StatePop[(Op, Fitness)]], Option[(Op, Fitness)]), EvalCDGPContinuous[Fitness]) = {
    (selection, evoMode) match {
      case ("tournament", "generational") =>
        val eval = getEvalForMSE(state, method)
        val alg = CDGPGenerationalTournament(eval)
        val finalPop = Main.watchTime(alg, RunExperiment(alg))
        ((finalPop, alg.bsf.bestSoFar), eval.asInstanceOf[EvalCDGPContinuous[Fitness]])

      case ("tournament", "steadyState") =>
        val eval = getEvalForMSE(state, method)
        val alg = CDGPSteadyStateTournament(eval)
        val finalPop = Main.watchTime(alg, RunExperiment(alg))
        ((finalPop, alg.bsf.bestSoFar), eval.asInstanceOf[EvalCDGPContinuous[Fitness]])

      case ("lexicase", "generational") =>
        val eval = getEvalForSeqDouble(state, method)
        val alg = CDGPGenerationalEpsLexicase(eval)
        val finalPop = Main.watchTime(alg, RunExperiment(alg))
        ((finalPop, alg.bsf.bestSoFar), eval.asInstanceOf[EvalCDGPContinuous[Fitness]])

      case ("lexicase", "steadyState") =>
        val eval = getEvalForSeqDouble(state, method)
        val alg = CDGPSteadyStateEpsLexicase(eval)
        val finalPop = Main.watchTime(alg, RunExperiment(alg))
        ((finalPop, alg.bsf.bestSoFar), eval.asInstanceOf[EvalCDGPContinuous[Fitness]])
    }
  }



  def run(implicit opt: Options): Unit = {
    assert(!opt('parEval, false), "CDGP does not support multithreaded evaluation.")
    implicit val coll = CollectorFile(opt)
    implicit val rng = Rng(opt)

    try {
      val benchmark = opt('benchmark)
      println(s"Benchmark: $benchmark")

      val method = opt('method)
      val selection = opt('selection, "lexicase")
      val evoMode = opt('evolutionMode, "steadyState")
      assert(method == "CDGP" || method == "GP" || method == "CDGPprops", s"Invalid method '$method'! Possible values: 'CDGP', 'CDGPprops', 'GP'.")
      assert(selection == "tournament" || selection == "lexicase",
        s"Invalid selection: '$selection'! Possible values: 'tournament', 'lexicase'.")
      assert(evoMode == "generational" || evoMode == "steadyState",
        s"Invalid evolutionMode: '$evoMode'! Possible values: 'generational', 'steadyState'.")

      // Create CDGP state
      val cdgpState = StateCDGP(benchmark)

      // Run algorithm
      val (res, eval) = runConfigRegressionCDGP(cdgpState, method, selection, evoMode)
      analyzeAndPrintResults(cdgpState, eval, res)
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
        coll.deleteSnapshot("cdgp") // Remove any .cdgp file if it was created
        coll.saveSnapshot("cdgp.error")
        e.printStackTrace()
    }
  }


  def analyzeAndPrintResults(cdgpState: StateCDGP,
                             eval: EvalCDGPContinuous[Fitness],
                             res: (Option[StatePop[(Op, Fitness)]], Option[(Op, Fitness)]))
                            (implicit opt: Options, coll: Collector): Unit = {
    val bestOfRun = res._2
    val (bestOp, _) = bestOfRun.get

    // Verify correctness with respect to the specification
    if (cdgpState.sygusData.formalConstr.nonEmpty) {
      val (dec, model) = cdgpState.verify(bestOp)
      coll.setResult("best.verificationDecision", dec)
      coll.setResult("best.verificationModel", model)
    }

    val eTrain = eval.evalOnTests(bestOfRun.get._1, cdgpState.trainingSet)
    val mseTrain = Tools.mse(eTrain)
    val eTest = eval.evalOnTests(bestOfRun.get._1, cdgpState.testSet)
    coll.setResult("best.trainEval", eTrain)
    coll.setResult("best.trainMSE", Tools.double2str(mseTrain))
    coll.setResult("best.testEval", if (eTest.nonEmpty) eTest else "n/a")
    coll.setResult("best.testMSE", if (eTest.nonEmpty) Tools.double2str(Tools.mse(eTest)) else "n/a")

    val dec = coll.getResult("best.verificationDecision").getOrElse("n/a")
    val e = eval.evalOnTestsAndConstraints(bestOfRun.get._1, cdgpState.testsManager.tests)
    coll.set("result.best.correctTests", eval.isOptimalOnCompleteTests(e, cdgpState.testsManager.tests))
    coll.set("result.best.correctVerification", if (dec == "unsat") true else false)

    // Save information about which constraints were passed
    if (opt('logPassedConstraints, false)) {
      // Create a 0/1 vector indicating if the ith constraint was passed
      // 1 means that the constraint was passed
      val passed = cdgpState.sygusData.formalConstr.map{ constr =>
        val template = new TemplateVerification(cdgpState.sygusData, false, cdgpState.timeout, Some(Seq(constr)))
        val (dec, _) = cdgpState.verify(bestOp, template)
        if (dec == "unsat") 0 else 1  // 0 means passed
      }
      coll.setResult("best.passedConstraints", passed)
    }

    // Print and save results
    coll.saveSnapshot("cdgp")
    printResults(cdgpState, bestOfRun)
  }


  def printResults(cdgpState: State, bestOfRun: Option[(Op, Fitness)])
                  (implicit coll: Collector, opt: Options) {
    assume(bestOfRun.isDefined, "No solution (optimal or approximate) to the problem was found.")
    def isOptimal(bestOfRun: (Op, Fitness)): Boolean = bestOfRun._2.correct

    val pn = 26
    println("\n")
    println("Best program found:".padTo(pn, ' ') + coll.getResult("bestOrig.smtlib").getOrElse("n/a"))
    println("Simplified:".padTo(pn, ' ') + coll.getResult("best.smtlib").getOrElse("n/a"))
    println("Evaluation:".padTo(pn, ' ') + coll.getResult("best.eval").getOrElse("n/a"))
    val dec = coll.getResult("best.verificationDecision").getOrElse("n/a")
    val model = coll.getResult("best.verificationModel").getOrElse("n/a")
    println("Correct on tests:".padTo(pn, ' ') + coll.get("result.best.correctTests").getOrElse("n/a"))
    println("Correct on verification:".padTo(pn, ' ') + coll.get("result.best.correctVerification").getOrElse("n/a"))
    println("Final verification:".padTo(pn, ' ') + s"$dec, model: $model")
    println("Program size:".padTo(pn, ' ') + coll.getResult("best.size").getOrElse("n/a"))
    println("MSE thresh:".padTo(pn, ' ') + coll.get("cdgp.optThresholdMSE").get)
    println("MSE (train):".padTo(pn, ' ') + coll.getResult("best.trainMSE").getOrElse("n/a"))
    println("MSE (test):".padTo(pn, ' ') + coll.getResult("best.testMSE").getOrElse("n/a"))
    println("Tests total:".padTo(pn, ' ') + coll.get("tests.total").getOrElse("n/a"))
    println("Tests known outputs:".padTo(pn, ' ') + coll.get("tests.totalKnownOutputs").getOrElse("n/a"))
    println("Total solver calls:".padTo(pn, ' ') + coll.get("solver.totalCalls").getOrElse("n/a"))
    println("Generation (best):".padTo(pn, ' ') + coll.getResult("best.generation").getOrElse("n/a"))
    println("Total generations:".padTo(pn, ' ') + coll.getResult("totalGenerations").getOrElse("n/a"))
    println("Total time [s]:".padTo(pn, ' ') + coll.getResult("totalTimeSystem").get.toString.toDouble / 1000.0)
    println("Log file:".padTo(pn, ' ') + coll.get("thisFileName").getOrElse("n/a"))

    if (opt("printTests", false)) {
      println("\nCollected tests:")
      cdgpState.testsManager.tests.foreach(println(_))
    }

    if (opt('logPassedConstraints, false)) {
      println("\nPassed constraints (0 = passed):")
      println(coll.get("result.best.passedConstraints").getOrElse("n/a"))
    }

    val sol = coll.getResult("best.smtlib").get.toString
    val solutionFull = SMTLIBFormatter.synthSolutionToString(cdgpState.synthTask, sol)

    println("\nOPTIMAL SOLUTION:")
    if (isOptimal(bestOfRun.get))
      println(solutionFull) else println("unknown")

    if (!isOptimal(bestOfRun.get)) {
      println(s"\nAPPROXIMATED SOLUTION:")
      println(solutionFull)
    }
  }



  // --------------------------------------------------------------------------
  //                                 MAIN
  // --------------------------------------------------------------------------

  def main(args: Array[String]): Unit = {
    if (Main.systemOptions(args))
      sys.exit()
    val opt = Main.getOptions(args ++ Array("--parEval", "false")) // ensure that --parEval false is used
    run(opt)
  }

}

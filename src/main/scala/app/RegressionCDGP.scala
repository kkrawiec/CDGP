package app

import java.io.{PrintWriter, StringWriter}

import cdgp._
import fuel.core.StatePop
import fuel.func._
import fuel.util._
import swim.tree.Op

import scala.util.Random


object RegressionCDGP {

  def getEvalForMSE(state: StateCDGP, method: String)
                   (implicit coll: Collector, opt: Options, rng: TRandom):
  EvalCDGPContinuous[FDouble] = {

    if (Set("CDGP", "CDGPprops").contains(method)) {
      val testsTypesForRatio = opt('testsTypesForRatio, "i").split(",").toSet
      EvalContinuous.EvalCDGPDoubleMSE(state, testsTypesForRatio)
    }
    else {
      val testsTypesForRatio = opt('testsTypesForRatio, "i").split(",").toSet
      EvalContinuous.EvalGPDoubleMSE(state, testsTypesForRatio)
    }
  }

  def getEvalForSeqDouble(state: StateCDGP, method: String)
                         (implicit coll: Collector, opt: Options, rng: TRandom):
  EvalCDGPContinuous[FSeqDouble] = {
    if (Set("CDGP", "CDGPprops").contains(method)) {
      val testsTypesForRatio = opt('testsTypesForRatio, "i").split(",").toSet
      EvalContinuous.EvalCDGPSeqDouble(state, testsTypesForRatio)
    }
    else {
      val testsTypesForRatio = opt('testsTypesForRatio, "i").split(",").toSet
      EvalContinuous.EvalGPSeqDouble(state, testsTypesForRatio)
    }
  }

  def createValidationSetTermination[E <: Fitness](state: StateCDGP, eval: EvalCDGPContinuous[E])
                                                  (implicit opt: Options, coll: Collector): ValidationSetTerminationHandler[E] = {
    if (opt.paramInt('sizeValidationSet) == 0)
      NoValidationSetTermination[E]()
    else
      new ValidationSetTermination[E](state.trainingSet, state.validationSet, eval.evaluatorComplete, opt('notImprovedWindow, 50), opt('reportFreq, 10))
  }

  def runConfigRegressionCDGP(state: StateCDGP, method: String, selection: String, evoMode: String)
                             (implicit coll: Collector, opt: Options, rng: TRandom):
  ((Option[StatePop[(Op, Fitness)]], Option[(Op, Fitness)]), EvalCDGPContinuous[Fitness], ValidationSetTerminationHandler[Fitness]) = {
    (selection, evoMode) match {
      case ("tournament", "generational") =>
        val eval = getEvalForMSE(state, method)
        val validTermination = createValidationSetTermination(state, eval)
        val alg = CDGPGenerationalTournament(eval, validTermination)
        val finalPop = Main.watchTime(alg, RunExperiment(alg))
        ((finalPop, alg.bsf.bestSoFar), eval.asInstanceOf[EvalCDGPContinuous[Fitness]], validTermination.asInstanceOf[ValidationSetTerminationHandler[Fitness]])

      case ("tournament", "steadyState") =>
        val eval = getEvalForMSE(state, method)
        val validTermination = createValidationSetTermination(state, eval)
        val alg = CDGPSteadyStateTournament(eval, validTermination)
        val finalPop = Main.watchTime(alg, RunExperiment(alg))
        ((finalPop, alg.bsf.bestSoFar), eval.asInstanceOf[EvalCDGPContinuous[Fitness]], validTermination.asInstanceOf[ValidationSetTerminationHandler[Fitness]])

      case ("lexicase", "generational") =>
        val eval = getEvalForSeqDouble(state, method)
        val validTermination = createValidationSetTermination(state, eval)
        val alg = CDGPGenerationalEpsLexicase(eval, validTermination)
        val finalPop = Main.watchTime(alg, RunExperiment(alg))
        ((finalPop, alg.bsf.bestSoFar), eval.asInstanceOf[EvalCDGPContinuous[Fitness]], validTermination.asInstanceOf[ValidationSetTerminationHandler[Fitness]])

      case ("lexicase", "steadyState") =>
        val eval = getEvalForSeqDouble(state, method)
        val validTermination = createValidationSetTermination(state, eval)
        val alg = CDGPSteadyStateEpsLexicase(eval, validTermination)
        val finalPop = Main.watchTime(alg, RunExperiment(alg))
        ((finalPop, alg.bsf.bestSoFar), eval.asInstanceOf[EvalCDGPContinuous[Fitness]], validTermination.asInstanceOf[ValidationSetTerminationHandler[Fitness]])
    }
  }


  def run(args: Array[String]): Unit = {
    val opt = Main.getOptions(args ++ Array("--parEval", "false"), group=Some(CDGPOptions.groupCDSR))
    run(opt)
  }


  def run(implicit opt: Options): Unit = {
    assert(!opt('parEval, false), "CDGP does not support multithreaded evaluation.")
    implicit val coll = CollectorFile(opt)
    implicit val rng = Rng(opt)

    Random.setSeed(opt('seed, 0))

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
      val (res, eval, validTermination) = runConfigRegressionCDGP(cdgpState, method, selection, evoMode)
      analyzeAndPrintResults(cdgpState, eval, res, validTermination)
    }
    catch {
      case e: NoSolutionException =>
        println(s"There is no solution to this problem.")
        println(s"Input with no correct answer: " + e.badInput)
        coll.set("cdgp.noCorrectSolution", true)
        coll.set("terminatingException", e.toString)
        coll.saveSnapshot("cdgp")
      case e: InitializationFailedException =>
        println(s"Initialization of the population have not finished properly. Often the reason is a very strict time limit.")
        coll.set("terminatingException", e.toString)
        coll.deleteSnapshot("cdgp") // Remove the .cdgp file if it was created
        coll.saveSnapshot("cdgp.error")
      case e: Throwable =>
        println(s"Terminating exception occurred! Message: ${e.getMessage}")
        coll.set("terminatingException", e.toString)
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        coll.set("terminatingExceptionStacktrace", sw.toString)
        coll.deleteSnapshot("cdgp") // Remove the .cdgp file if it was created
        coll.saveSnapshot("cdgp.error")
        e.printStackTrace()
    }
  }

  def chooseBestOnValidSet(cdgpState: StateCDGP,
                           eval: EvalCDGPContinuous[Fitness],
                           bestOnTrain: (Op, Fitness),
                           originalBestOnValid: Option[(Op, Double)]): Op = {
    if (originalBestOnValid.isEmpty)
      bestOnTrain._1
    else {
      val (opTrain, _) = bestOnTrain
      val (opValid, _) = originalBestOnValid.get
      val evalTrainMSE = Tools.mse(eval.evalOnTests(opTrain, cdgpState.validationSet))
      val evalValidMSE = Tools.mse(eval.evalOnTests(opValid, cdgpState.validationSet))
      if (evalTrainMSE < evalValidMSE) // better MSE on train set happens as far as I know only when a perfect solution was found, search was terminated, and best on validation set was not updated
        opTrain
      else
        opValid
    }
  }

  def analyzeAndPrintResults(cdgpState: StateCDGP,
                             eval: EvalCDGPContinuous[Fitness],
                             res: (Option[StatePop[(Op, Fitness)]], Option[(Op, Fitness)]),
                             validTermination: ValidationSetTerminationHandler[Fitness])
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
    val mseTest = Tools.mse(eTest)
    val eValid = eval.evalOnTests(bestOfRun.get._1, cdgpState.validationSet)
    val mseValid = Tools.mse(eValid)
    coll.setResult("best.trainEval", eTrain)
    coll.setResult("best.trainMSE", Tools.double2str(mseTrain))
    coll.setResult("best.testEval", if (eTest.nonEmpty) eTest else "n/a")
    coll.setResult("best.testMSE", if (eTest.nonEmpty) Tools.double2str(mseTest) else "n/a")
    coll.setResult("best.validEval", if (eValid.nonEmpty) eValid else "n/a")
    coll.setResult("best.validMSE", if (eValid.nonEmpty) Tools.double2str(mseValid) else "n/a")

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
        if (dec == "unsat") 0 else 1  // 0 means a constraint was passed
      }
      coll.setResult("best.passedConstraints", passed)
    }


    // Logging the same information as earlier but this time for the solution best on the validation set
    if (validTermination.isUsed) {  // "&& validTermination.bsfValid.isDefined" - the case of bsfValid being None is handled in the line below
      val vOp = chooseBestOnValidSet(cdgpState, eval, bestOfRun.get, validTermination.bsfValid)

      // Verify correctness with respect to the specification
      if (cdgpState.sygusData.formalConstr.nonEmpty) {
        val (dec, model) = cdgpState.verify(vOp)
        coll.setResult("validation.best.verificationDecision", dec)
        coll.setResult("validation.best.verificationModel", model)
      }

      val eTrain = eval.evalOnTests(vOp, cdgpState.trainingSet)
      val mseTrain = Tools.mse(eTrain)
      val eTest = eval.evalOnTests(vOp, cdgpState.testSet)
      val mseTest = Tools.mse(eTest)
      val eValid = eval.evalOnTests(vOp, cdgpState.validationSet)
      val mseValid = Tools.mse(eValid)
      coll.setResult("validation.best.mse", Tools.double2str(mseTrain))
      coll.setResult("validation.best.trainEval", eTrain)
      coll.setResult("validation.best.trainMSE", Tools.double2str(mseTrain))
      coll.setResult("validation.best.testEval", if (eTest.nonEmpty) eTest else "n/a")
      coll.setResult("validation.best.testMSE", if (eTest.nonEmpty) Tools.double2str(mseTest) else "n/a")
      coll.setResult("validation.best.validEval", if (eValid.nonEmpty) eValid else "n/a")
      coll.setResult("validation.best.validMSE", if (eValid.nonEmpty) Tools.double2str(mseValid) else "n/a")

      val vSolutionOrigOp = vOp
      val vSolutionOrigSmtlib = SMTLIBFormatter.opToSmtlib(vOp)
      val vSolutionSimpSmtlib = cdgpState.simplifySolution(vSolutionOrigSmtlib).getOrElse(vSolutionOrigSmtlib)
      val vSolutionSimpOp = SMTLIBFormatter.smtlibToOp(vSolutionSimpSmtlib)


      // Simplified best on valid
      coll.set("result.validation.best", vSolutionSimpOp)
      coll.set("result.validation.best.smtlib", vSolutionSimpSmtlib)
      coll.set("result.validation.best.size", vSolutionSimpOp.size)
      coll.set("result.validation.best.height", vSolutionSimpOp.height)

      // Original best on valid
      coll.set("result.validation.bestOrig", vSolutionOrigOp)
      coll.set("result.validation.bestOrig.smtlib", vSolutionOrigSmtlib)
      coll.set("result.validation.bestOrig.size", vSolutionOrigOp.size)
      coll.set("result.validation.bestOrig.height", vSolutionOrigOp.height)


      val dec = coll.getResult("validation.best.verificationDecision").getOrElse("n/a")
      val e = eval.evalOnTestsAndConstraints(vOp, cdgpState.testsManager.tests)
      coll.set("result.validation.best.correctTests", eval.isOptimalOnCompleteTests(e, cdgpState.testsManager.tests))
      coll.set("result.validation.best.correctVerification", if (dec == "unsat") true else false)

      // Save information about which constraints were passed
      if (opt('logPassedConstraints, false)) {
        // Create a 0/1 vector indicating if the ith constraint was passed
        // 1 means that the constraint was passed
        val passed = cdgpState.sygusData.formalConstr.map{ constr =>
          val template = new TemplateVerification(cdgpState.sygusData, false, cdgpState.timeout, Some(Seq(constr)))
          val (dec, _) = cdgpState.verify(vOp, template)
          if (dec == "unsat") 0 else 1  // 0 means a constraint was passed
        }
        coll.setResult("validation.best.passedConstraints", passed)
      }

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

    if (coll.getResult("best.smtlib").isEmpty)
      println("\nThe best of run solution is unavailable! This usually happens when the initial population is not completely evaluated because of the set time limit.")
    else {
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
  }



  // --------------------------------------------------------------------------
  //                                 MAIN
  // --------------------------------------------------------------------------

  def main(args: Array[String]): Unit = {
    if (Main.systemOptions(args))
      sys.exit()
    run(args)
  }

}

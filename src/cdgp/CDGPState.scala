package cdgp

import fuel.util.{Collector, Options, TRandom}
import swim.Grammar
import swim.tree.Op
import sygus16.SyGuS16


class NoSolutionException(val badInput: String) extends Exception {
  override def toString: String = s"NoSolutionException($badInput)"
}


/**
  * Manages everything needed for the CDGP to run. Among other things, handles interaction
  * with the solver, and contains test manager. As argument given is the definition of the
  * SYGUS problem to be solved, and from it extracted are all important information such
  * as grammar and logic to be used.
  */
class CDGPState(val sygusProblem: SyGuS16)
               (implicit opt: Options, coll: Collector, rng: TRandom) {
  // The types for input and output
  type I = Map[String, Any]
  type O = Any
  val testsManager = new TestsManagerCDGP[I, O](opt("logTestsHistory", false), opt("printTests", false))

  val method = opt('method, "CDGP")
  val searchAlg = opt('searchAlgorithm)
  assert(method == "CDGP" || method == "GPR", s"Invalid method '$method'! Possible values: 'CDGP', 'GPR'.")
  assert(searchAlg == "GP" || searchAlg == "GPSteadyState" ||
         searchAlg == "Lexicase" || searchAlg == "LexicaseSteadyState",
         s"Invalid searchAlgorithm: '$searchAlg'! Possible values: 'GP', 'GPSteadyState', 'Lexicase', 'LexicaseSteadyState'.")

  // Other parameters
  val GPRminInt: Int = opt('GPRminInt, -100)
  val GPRmaxInt: Int = opt('GPRmaxInt, 100)
  val CDGPtestsRatio: Double = opt('CDGPtestsRatio, 1.0, (x: Double) => x >= 0.0 && x <= 1.0)
  val GPRtestsRatio: Double = opt('GPRtestsRatio, 1.0, (x: Double) => x >= 0.0 && x <= 1.0)
  val maxNewTestsPerIter: Int = opt('maxNewTestsPerIter, Int.MaxValue, (x: Int) => x > 0)
  val timeout: Int = opt('solverTimeout, if (opt('solverType, "z3") == "z3") 5000 else 0)
  val silent = opt('silent, false)
  val alwaysSearchForOutput = opt('alwaysSearchForOutput, true)
  val gprRetryIfUndefined = opt('gprRetryIfUndefined, true)


  val sygusData = SygusProblemData(sygusProblem, opt('mixedSpecAllowed, true))
  val invocations: Seq[Seq[String]] = sygusData.formalInvocations
  def synthTask: SygusSynthesisTask = sygusData.synthTask
  val grammar: Grammar = synthTask.getSwimGrammar(rng)
  val singleAnswerFormal: Boolean = sygusData.singleInvocFormal && hasSingleAnswerForEveryInput(sygusProblem).getOrElse(false)

  // Initializing population of test cases
  testsManager.addNewTests(sygusData.testCasesConstrToTests)
  // testsManager.flushHelpers() // This is done elsewhere


  /*
   * Depending on the properties of the problem, CDGPState will switch between using
   * GP domain and executing the solver for computing fitness.
   */
  val evaluationMode: String = getEvaluationMode
  assert(evaluationMode == "solver" || evaluationMode == "gp")
  val useDomainEvaluation: Boolean = evaluationMode == "gp"
  println(s"(evaluationMode $evaluationMode)")
  coll.set("cdgp.evaluationMode", evaluationMode)


  // Currently the domain is hardcoded. This matters only for problems which
  // can be domain-evaluated.
  val domain = SLIA(synthTask.argNames, Symbol(synthTask.fname), opt("recDepthLimit", 1000))


  // Creating solver manager
  private def solverPath = opt('solverPath)
  private def solverArgs = opt.getOption("solverArgs")
  private def moreSolverArgs = opt.getOption("moreSolverArgs", "")
  lazy val solver = new SolverManager(solverPath, solverArgs, moreSolverArgs, verbose=false)

  // Templates for solver queries
  lazy val templateVerification = new TemplateVerification(sygusProblem, sygusData, timeout = timeout)
  lazy val templateIsOutputCorrectForInput = new TemplateIsOutputCorrectForInput(sygusProblem, sygusData, timeout = timeout)
  lazy val templateIsProgramCorrectForInput = new TemplateIsProgramCorrectForInput(sygusProblem, sygusData, timeout = timeout)
  lazy val templateFindOutput = new TemplateFindOutput(sygusProblem, sygusData, timeout = timeout)
  lazy val templateFindOutputNeg = new TemplateFindOutput(sygusProblem, sygusData, negateConstr = true, timeout = timeout)
  lazy val templateSimplify = new TemplateSimplify(sygusProblem, sygusData, timeout = timeout)


  // For statistic/diagnostic info
  var numRejectedCounterex = 0


  def getEvaluationMode: String = {
    println(s"(singleInvocationProperty ${sygusData.singleInvocFormal})")
    coll.set("cdgp.singleInvocationProperty", sygusData.singleInvocFormal)
    if (sygusData.singleInvocFormal) {
      // Checking for the single answer property has sense only if the problem
      // has single invocation property.

      val singleAnswerText = singleAnswerFormal
      println(s"(singleAnswerForEveryInput $singleAnswerText)")
      println(s"(supportForAllTerms ${sygusData.supportForAllTerms})")
      coll.set("cdgp.singleAnswerForEveryInput", singleAnswerText)
      coll.set("cdgp.supportForAllTerms", sygusData.supportForAllTerms)
      if (singleAnswerFormal && sygusData.supportForAllTerms)
        "gp"  // we may be consider treating unknown singleAnswer as true, with the potential risk of losing "soundness" of the fitness
      else
        "solver"
    }
    else
      "solver"
  }


  /**
    * Tests a program on the available tests and returns the vector of 0s (passed test)
    * and 1s (failed test). Depending on the problem will either optimize by executing
    * program directly on the tests, or will have to resort to a solver.
    */
  def evalOnTests(s: Op, tests: Seq[(I, Option[O])]): Seq[Int] = {
    def handleException(test: (I, Option[O]), message: String) {
      val msg = s"Error during evalutation of $s and test $test: $message"
      coll.set("error_evalOnTests", msg)
      println(msg)
    }
    for (test <- tests) yield {
      try {
        if (test._2.isDefined)
          // User can define test cases for a problem, in which generally single-answer
          // property does not hold. We will use domain for those cases, since it is more
          // efficient.
          evalOnTestsDomain(s, test)
        else evalOnTestsSolver(s, test)
      }
      catch { case e: Throwable => handleException(test, e.getMessage); 1 }
    }
  }

  /**
    * Checks correctness of the program only for the given test.
    * Tests here always have None as the answer, because in general there is no
    * single answer for the problem being solved in 'solver' mode.
    */
  def evalOnTestsSolver(s: Op, test: (I, Option[O])): Int = {
    val testModel: Map[String, Any] = test._1
    val (dec, _) = checkIsProgramCorrectForInput(s, testModel)
    if (dec == "sat") 0 else 1
  }

  /**
    * Checks correctness of the program only for the given test.
    * If test has a defined expected answer, then it is compared with the answer
    * obtained by executing the program in the domain simulating semantics of SMT
    * theory.
    * If test don't have defined expected answer, then the program's output is verified
    * by the solver for consistency with the specification. The test will be updated if
    * this output is deemed consistent by the solver.
    *
    * Names of variables in test should be the same as those in the function's invocation.
    * They will be renamed for those in the function's declaration.
    */
  def evalOnTestsDomain(s: Op, test: (I, Option[O])): Int = {
    val testModel: Map[String, Any] = test._1
    val testOutput: Option[Any] = test._2
    val testInputsRenamed = modelToSynthFunInputs(testModel)
    val inputVector = synthTask.argNames.map(testInputsRenamed(_))
    val output = domain(s)(inputVector)
    if (output.isEmpty)
      1  // None means that recurrence depth was exceeded
    else if (testOutput.isDefined) {
      if (output.get == convertValue(testOutput.get)) 0 else 1
    }
    else {
      // Situation, when the test case has None as the expected output
      // We don't allow such a situation
      throw new Exception("Trying to domain-evaluate a test without defined correct answer!")

      // The code below can be used to try to find the expected output for the test
      //val (dec, _) = checkIsOutputCorrectForInput(s, testModel, output.get)
      //if (dec == "sat")
      //  testsManager.updateTest((testModel, output))
      //if (dec == "sat") 0 else 1
    }
  }

  def convertValue(value: Any): Any =
    value match {
      case str: String => Tools.convertToJavaString(str)
      case _ => value
    }

  /**
    * Transforms a model returned by the solver to a mapping from synth-fun argument name
    * to value derived from the model.
    */
  def modelToSynthFunInputs(testModel: Map[String, Any]): Map[String, Any] =
    SygusUtils.renameVars(testModel, invocations.head, synthTask.argNames)


  ///////  Interactions with the solver  ///////

  /**
    * Checks using SMT solver if the given problem has only one correct answer for
    * any input.
    */
  def hasSingleAnswerForEveryInput(problem: SyGuS16): Option[Boolean] = {
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(problem, sygusData, solverTimeout=timeout)
    // println("\nQuery checkIfSingleAnswerForEveryInput:\n" + query)
    val (dec, model) = solver.runSolver(query)
    if (dec == "sat") {
      val values = GetValueParser(model.get)
      if (!silent) println("Example of multiple correct answers: " + values.mkString(" "))
      Some(false)
    }
    else if (dec == "unsat") Some(true)
    else None
  }

  def verify(s: Op): (String, Option[String]) = {
    val query = templateVerification(s)
    // println("\nQuery verify:\n" + query)
    solver.runSolver(query)
  }

  def checkIsOutputCorrectForInput(s: Op,
                                   testInputsMap: Map[String, Any],
                                   output: Any): (String, Option[String]) = {
    val query = templateIsOutputCorrectForInput(testInputsMap, output)
    // println("\nQuery checkOnInputAndKnownOutput:\n" + query)
    solver.runSolver(query)
  }

  def checkIsProgramCorrectForInput(s: Op,
                                    testModel: Map[String, Any]): (String, Option[String]) = {
    val query = templateIsProgramCorrectForInput(s, testModel)
    // println("\nQuery checkOnInputOnly:\n" + query)
    solver.runSolver(query)
  }

  def findOutputForTestCase(test: (I, Option[O]), singleAnswer: Boolean = true): (I, Option[O]) = {
    assert(test._2.isEmpty)
    if (!sygusData.singleInvocFormal || !sygusData.supportForAllTerms)
      test
    else {
      try {
        val query = templateFindOutput(test._1)
        // println("\nQuery findOutputForTestCase:\n" + query)
        val (dec, res) = solver.runSolver(query)
        if (dec == "sat") {
          val output: Option[Any] = GetValueParser(res.get).toMap.get(TemplateFindOutput.CORRECT_OUTPUT_VAR)
          if (singleAnswer) // we have guarantees that the found output is the only correct one
            (test._1, output)

          else if (alwaysSearchForOutput) {
            // We are trying to find some other output which satisfies the constraints.
            // If we succeed, then test must be evaluated by solver in the future, hence the None
            // ain place of correct output.
            //
            // This can be especially useful if outputs for some parts of input space are undefined.
            // Undefinedness, which means that for such inputs every output is acceptable, makes the whole
            // problem to not have the single-answer property, even if otherwise all inputs have single-answer.
            val query2 = templateFindOutput(test._1, excludeValues = List(output.get))
            // println("\nQuery findOutputForTestCase2:\n" + query2)
            val (dec2, res2) = solver.runSolver(query2)
            if (dec2 == "unsat")
              (test._1, output)
            else
              test
          }

          else
            test
        }
        else if (dec == "unsat")
          throw new NoSolutionException(test._1.toString)
        else  // e.g. unknown
          test
      }
      catch {
        case _: Throwable =>
          println(s"Exception during executing query or parsing result, returning test with no output! ")
          test
      }
    }
  }

  def simplifySolution(smtlib: String): Option[String] = {
    try {
      val query = templateSimplify(smtlib)
      val res = solver.executeQuery(query)
      if (res.trim.startsWith("(error")) None else Some(res)
    }
    catch { case _: Throwable => None }
  }

  ///////////////////////////////////////////////////////




  def createTestFromFailedVerification(verOutput: String): Option[(Map[String, Any], Option[Any])] = {
    try {
      val testModel = GetValueParser(verOutput) // parse model returned by solver
      val testNoOutput = (testModel.toMap, None) // for this test currently the correct answer is not known

      if (testsManager.tests.contains(testNoOutput._1))
        None // this input already was used, there is no use to search for output
      else {
        Some(findOutputForTestCase(testNoOutput, singleAnswerFormal))
      }
    } catch {
      case e: Throwable =>
        println(s"Error during creation of counterexample from: $verOutput\nOriginal message: " + e.getMessage)
        numRejectedCounterex += 1
        None
    }
  }

  def createRandomTest(): Option[(Map[String, Any], Option[Any])] = {
    val example = sygusData.varDeclsNames.map(a => (a, GPRminInt + rng.nextInt(GPRmaxInt+1-GPRminInt)))
    val testNoOutput = (example.toMap, None) // for this test currently the correct answer is not known
    if (testsManager.tests.contains(testNoOutput._1))
      createRandomTest() // try again
    else if (gprRetryIfUndefined) {
      // We will now check if for the input there exists an incorrect output.
      // This is necessary in case GPR generated a test with undefined answer.
      // Adding such a test is meaningless.
      val query = templateFindOutputNeg(testNoOutput._1)
      val (dec, _) = solver.runSolver(query)
      if (dec == "unsat")
        createRandomTest() // try again
      else
        Some(findOutputForTestCase(testNoOutput, singleAnswerFormal))
    }
    else
      Some(findOutputForTestCase(testNoOutput, singleAnswerFormal))
  }

  /**
    * Creates CDGPEvaluation based on options.
    */
  def getCDGPEvaluation[S, E](eval: S => E): CDGPEvaluation[S, E] =
    new CDGPEvaluation(this, eval)
  /**
    * Creates CDGPEvaluationSteadyState based on options.
    */
  def getCDGPEvaluationSteadyState[S, E](eval: S => E,
                                         updateEval: ((S, E)) => (S, E)): CDGPEvaluation[S, E] =
    new CDGPEvaluationSteadyState(this, eval, updateEval)


  val fitness: (Op) => (Boolean, Seq[Int]) =
    method match {
      case "CDGP"     => fitnessCDGPGeneral
      case "GPR"      => fitnessGPR
    }

  def fitnessNoVerification(s: Op): (Boolean, Seq[Int]) = {
    (false, evalOnTests(s, testsManager.getTests()))
  }

  /** Fitness is always computed on the tests that were flushed. */
  def fitnessCDGPGeneral: Op => (Boolean, Seq[Int]) =
    new Function1[Op, (Boolean, Seq[Int])] {
      def doVerify(evalTests: Seq[Int]): Boolean = {
        val numPassed = evalTests.count(_ == 0).asInstanceOf[Double]
        (numPassed / evalTests.size) >= CDGPtestsRatio || evalTests.isEmpty
      }
      def apply(s: Op): (Boolean, Seq[Int]) = {
        val evalTests = evalOnTests(s, testsManager.getTests())
        // If the program passes the specified ratio of test cases, it will be verified
        // and a counterexample will be produced (or program will be deemed correct).
        // NOTE: if the program does not pass all test cases, then the probability is high
        // that the produced counterexample will already be in the set of test cases.
        if (!doVerify(evalTests))
          (false, evalTests)
        else {
          val (decision, r) = verify(s)
          if (decision == "unsat" && evalTests.sum == 0 && (!(sygusData.sygusLogic == "SLIA") || evalTests.nonEmpty) )
            (true, evalTests)  // perfect program found; end of run
          else if (decision == "sat") {
            if (testsManager.newTests.size < maxNewTestsPerIter) {
              val newTest = createTestFromFailedVerification(r.get)
              if (newTest.isDefined)
                testsManager.addNewTest(newTest.get)
            }
            (false, evalTests)
          }
          else {
            // The 'unknown' or 'timeout' solver's decision. Program potentially may be the optimal
            // solution, but solver is not able to verify this. We proceed by adding no new tests
            // and treating the program as incorrect.
            (false, evalTests)
          }
        }
      }
    }

  def fitnessGPR: Op => (Boolean, Seq[Int]) = {
    new Function1[Op, (Boolean, Seq[Int])] {
      def doSearchForCounterexample(evalTests: Seq[Int]): Boolean = {
        val numPassed = evalTests.count(_ == 0).asInstanceOf[Double]
        (numPassed / evalTests.size) >= GPRtestsRatio || evalTests.isEmpty
      }
      def allTestsPassed(evalTests: Seq[Int]): Boolean =
        evalTests.count(_ == 0) == evalTests.size
      def generateAndAddRandomTest(): Unit = {
        if (testsManager.newTests.size < maxNewTestsPerIter) {
          val newTest = createRandomTest()
          if (newTest.isDefined)
            testsManager.addNewTest(newTest.get)
        }
      }
      def apply(s: Op): (Boolean, Seq[Int]) = {
        val evalTests = evalOnTests(s, testsManager.getTests())
        if (!doSearchForCounterexample(evalTests))
          (false, evalTests)
        else if (allTestsPassed(evalTests)) {
          // program passes all tests - verify if it is correct
          val (decision, _) = verify(s)
          if (decision == "unsat" && evalTests.sum == 0 && (!(sygusData.sygusLogic == "SLIA") || evalTests.nonEmpty))
            (true, evalTests)  // perfect program found; end of run
          else {
            generateAndAddRandomTest()  // program incorrect; generate random test
            (false, evalTests)
          }
        }
        else {  // program passes enough tests but not all - generate random counterexample
          generateAndAddRandomTest()
          (false, evalTests)
        }
      }
    }
  }

  def updateEvalInt(s: (Op, FInt)): (Op, FInt) = {
    val newFit = FInt(s._2.correct, s._2.value + evalOnTests(s._1, testsManager.newTests.toList).sum, s._1.size)
    (s._1, newFit)
  }
  def updateEvalSeqInt(s: (Op, FSeqInt)): (Op, FSeqInt) =
    (s._1, FSeqInt(s._2.correct, s._2.value ++ evalOnTests(s._1, testsManager.newTests.toList), s._1.size))
}


object CDGPState {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPState =
    new CDGPState(LoadSygusBenchmark(benchmark))

  def apply(sygusProblem: SyGuS16)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPState =
    new CDGPState(sygusProblem)
}

package cdgp

import fuel.util.{Collector, Options, TRandom}
import swim.Grammar
import swim.tree.Op
import sygus.{Cmd, ConstraintCmd, VarDeclCmd}
import sygus16.SyGuS16


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
  val testsManager = new TestsManagerCDGP[I, O](printAddedTests = opt("printTests", false))

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
  val timeout: Int = opt('solverTimeout, 0)
  val silent = opt('silent, false)


  val sygusData = SygusProblemData(sygusProblem, opt('mixedSpecAllowed, true))
  def synthTask: SygusSynthesisTask = sygusData.synthTask
  val invocations: Seq[Seq[String]] = sygusData.formalInvocations

  // Initializing population of test cases
  testsManager.addNewTests(sygusData.testCasesConstrToTests)
  // testsManager.flushHelpers()

  def grammar: Grammar = synthTask.grammar
  val varDecls: List[VarDeclCmd] = sygusProblem.cmds.collect { case v: VarDeclCmd => v }
  val varDeclsNames: Set[String] = varDecls.map(_.sym).toSet

  /*
   * Depending on the properties of the problem, CDGPState will switch between using
   * GP domain and executing the solver for computing fitness.
   */
  val testCasesMode: String = getTestCasesMode(sygusProblem)
  assert(testCasesMode == "solver" || testCasesMode == "gp")
  val useDomainToComputeFitness: Boolean = testCasesMode == "gp"
  println(s"(testCasesMode $testCasesMode)")
  coll.set("cdgp.testCasesMode", testCasesMode)
  if (!useDomainToComputeFitness)
    println("INFO: solver will be used to compute fitness. Expect major efficiency decrease" +
      " in comparison with GP test cases mode.")


  val domain = SLIA(synthTask.argNames, synthTask.fname, opt("recDepthLimit", 1000))


  // Pre- and post-conditions of the synthesis problem
  val pre: Seq[ConstraintCmd] = SygusUtils.getPreconditions(sygusProblem)
  val post: Seq[ConstraintCmd] = SygusUtils.getPostconditions(sygusProblem)
  if (!silent) {
    println("\nPRECONDITIONS:")
    pre.foreach { case ConstraintCmd(t) => println(SMTLIBFormatter.termToSmtlib(t)) }
    println("\nPOSTCONDITIONS:")
    post.foreach { case ConstraintCmd(t) => println(SMTLIBFormatter.termToSmtlib(t)) }
    println("")
  }



  // Creating solver manager
  private def solverPath = opt('solverPath)
  private def solverArgs = opt.getOption("solverArgs")
  private def moreSolverArgs = opt.getOption("moreSolverArgs", "")
  lazy val solver = new SolverManager(solverPath, solverArgs, moreSolverArgs, verbose=false)

  // Templates for solver queries
  val templateVerification = new TemplateVerification(sygusProblem, sygusData, timeout = timeout)
  val templateIsOutputCorrectForInput = new TemplateIsOutputCorrectForInput(sygusProblem, sygusData, timeout = timeout)
  val templateIsProgramCorrectForInput = new TemplateIsProgramCorrectForInput(sygusProblem, sygusData, timeout = timeout)
  val templateFindOutput = new TemplateFindOutput(sygusProblem, sygusData, timeout = timeout)
  val templateSimplify = new TemplateSimplify(sygusProblem, sygusData)


  def getTestCasesMode(problem: SyGuS16): String = {
    val singleInvoc = SygusUtils.hasSingleInvocationProperty(sygusData)
    println(s"(singleInvocationProperty $singleInvoc)")
    coll.set("cdgp.singleInvocationProperty", singleInvoc)
    if (singleInvoc) {
      // Checking for the single answer property has sense only if the problem
      // has single invocation property.
      val singleAnswer = hasSingleAnswerForEveryInput(problem)
      val supportForAllTerms = !SygusUtils.containsUnsupportedComplexTerms(problem)
      println(s"(singleAnswerForEveryInput ${singleAnswer.getOrElse("unknown")})")
      println(s"(supportForAllTerms $supportForAllTerms)")
      coll.set("cdgp.singleAnswerForEveryInput", singleAnswer.getOrElse("unknown"))
      coll.set("cdgp.supportForAllTerms", supportForAllTerms)
      if (singleAnswer.getOrElse(false) && supportForAllTerms)
        "gp"  // it may be considered to treat unknown singleAnswer as true, with the potential risk of losing "soundness" of the fitness
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
        if (useDomainToComputeFitness) evalOnTestsDomain(s, test)
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
    * Names of variables in test should be the same as those as in the function's invocation.
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
      if (output.get == testOutput.get) 0 else 1
    }
    else {
      val (dec, _) = checkIsOutputCorrectForInput(s, testModel, output.get)
      if (dec == "sat")
        testsManager.updateTest((testModel, output))
      if (dec == "sat") 0 else 1
    }
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
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(synthTask, problem)
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

  def findOutputForTestCase(test: (I, Option[O])): (I, Option[O]) = {
    val query = templateFindOutput(test._1)
    // println("\nQuery findOutputForTestCase:\n" + query)
    try {
      val (dec, res) = solver.runSolver(query)
      if (dec == "sat") {
        val values = GetValueParser(res.get)
        (test._1, Some(values.head._2))
      }
      else
        test
    }
    catch {
      case _: Throwable =>
        println(s"Exception during executing query or parsing result, returning test with no output! ")
        test // in case solver returns unknown
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




  def createTestFromFailedVerification(verOutput: String): (Map[String, Any], Option[Any]) = {
    val testModel = GetValueParser(verOutput)  // parse model returned by solver
    val testNoOutput = (testModel.toMap, None)  // for this test currently the correct answer is not known
    if (useDomainToComputeFitness)
      findOutputForTestCase(testNoOutput)
    else
      testNoOutput
  }

  def createRandomTest(): (Map[String, Any], Option[Any]) = {
    val example = varDeclsNames.map(a => (a, GPRminInt + rng.nextInt(GPRmaxInt+1-GPRminInt)))
    val testNoOutput = (example.toMap, None) // for this test currently the correct answer is not known
    if (useDomainToComputeFitness)
      findOutputForTestCase(testNoOutput)
    else
      testNoOutput
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
      def apply(s: Op) = {
        val evalTests = evalOnTests(s, testsManager.getTests())
        // If the program passes the specified ratio of test cases, it will be verified
        // and a counterexample will be produced (or program will be deemed correct).
        // NOTE: if the program does not pass all test cases, then the probability is high
        // that produced counterexample will already be in the set of test cases.
        if (!doVerify(evalTests))
          (false, evalTests)
        else {
          val (decision, r) = verify(s)
          if (decision == "unsat")
            (true, evalTests)  // perfect program found; end of run
          else if (decision == "sat") {
            if (testsManager.newTests.size < maxNewTestsPerIter) {
              val newTest = createTestFromFailedVerification(r.get)
              testsManager.addNewTest(newTest)
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
          testsManager.addNewTest(newTest)
        }
      }
      def apply(s: Op) = {
        val evalTests = evalOnTests(s, testsManager.getTests())
        if (!doSearchForCounterexample(evalTests))
          (false, evalTests)
        else if (allTestsPassed(evalTests)) {
          // program passes all tests - verify if it is correct
          val (decision, _) = verify(s)
          if (decision == "unsat")
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

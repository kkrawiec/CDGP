package cdgp

import fuel.util.{Collector, Options, TRandom}
import swim.tree.Op
import sygus.{BoolSortExpr, IntSortExpr, RealSortExpr, SortExpr}
import sygus16.SyGuS16


class NoSolutionException(val badInput: String) extends Exception {
  override def toString: String = s"NoSolutionException($badInput)"
}










abstract class State(val sygusData: SygusProblemData)
                    (implicit opt: Options, coll: Collector, rng: TRandom) {
  // The types for input and output
  type I = Map[String, Any]
  type O = Any
  val testsManager = new TestsManagerCDGP[I, O](opt("logTestsHistory", false), opt("printTests", false))
  val silent = opt('silent, false)

  /**
    * Saves state-related info and statistics in the collector.
    */
  def reportData(): Unit = {
    testsManager.reportData(coll)
  }

  /**
    * Converts a value to the format used in Java (necessary esp. for strings).
    */
  def convertValue(value: Any): Any =
    value match {
      case str: String => Tools.convertToJavaString(str)
      case _ => value
    }

  // Convenience methods
  def synthTask: SygusSynthTask = sygusData.synthTask
  def invocations: Seq[Seq[String]] = sygusData.formalInvocations
}
















class StateSMTSolver(sygusData: SygusProblemData)
                    (implicit opt: Options, coll: Collector, rng: TRandom)
  extends State(sygusData) {

  // Parameters
  val printQueries = opt('printQueries, false)
  val timeout: Int = opt('solverTimeout, if (opt('solverType, "z3") == "z3") 5000 else 0)
  private val solverPath = opt('solverPath)
  private val solverArgs = opt.getOption("solverArgs")
  private val moreSolverArgs = opt.getOption("moreSolverArgs", "")

  // Creating solver manager
  lazy val solver = new SolverManager(solverPath, solverArgs, moreSolverArgs, verbose=false)


  // Templates for solver queries
  lazy val templateVerification = new TemplateVerification(sygusData, timeout = timeout)
  lazy val templateIsOutputCorrectForInput = new TemplateIsOutputCorrectForInput(sygusData, timeout = timeout)
  lazy val templateIsProgramCorrectForInput = new TemplateIsProgramCorrectForInput(sygusData, timeout = timeout)
  lazy val templateFindOutput = new TemplateFindOutput(sygusData, timeout = timeout)
  lazy val templateFindOutputNeg = new TemplateFindOutput(sygusData, negateConstr = true, timeout = timeout)
  lazy val templateSimplify = new TemplateSimplify(sygusData, timeout = timeout)


  /**
    * Transforms a model returned by the solver to a mapping from synth-fun argument name
    * to value derived from the model.
    */
  def modelToSynthFunInputs(model: Map[String, Any]): Map[String, Any] =
    CDGPState.modelToSynthFunInputs(model, invocations.head, synthTask.argNames)


  /**
    * Creates a complete test, i.e. a test containing both input and the only correct
    * output for that input.
    * @param model Valuation of synth-variables. Will be converted to synth-fun inputs
    *              in test case.
    * @param output An output of the test case computed by the solver.
    */
  def createCompleteTest(model: Map[String, Any], output: Option[O]): CompleteTestCase[I, O] = {
    val synthFunInputs = modelToSynthFunInputs(model)
    CompleteTestCase[I, O](synthFunInputs, output)
  }



  ///////  Interactions with the solver  ///////

  /**
    * Checks using SMT solver if the given problem has only one correct answer for
    * all possible input.
    */
  def hasSingleAnswerForEveryInput(problem: SyGuS16): Option[Boolean] = {
    if (sygusData.formalConstr.isEmpty)
      Some(false)
    else {
      val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(problem, sygusData, solverTimeout = timeout)
      // printQuery("\nQuery checkIfSingleAnswerForEveryInput:\n" + query)
      val (dec, model) = solver.runSolver(query)
      if (dec == "sat") {
        val values = GetValueParser(model.get)
        if (!silent) println("Example of multiple correct answers: " + values.mkString(" "))
        Some(false)
      }
      else if (dec == "unsat") Some(true)
      else None
    }
  }


  /**
    * Verifies a program with respect to the specification.
    */
  def verify(s: Op): (String, Option[String]) = {
    val query = templateVerification(s)
    printQuery("\nQuery verify:\n" + query)
    solver.runSolver(query)
  }


  /**
    * Checks, if a particular output is consistent with the specification..
    */
  def checkIsOutputCorrect(s: Op,
                           testInputsMap: Map[String, Any],
                           output: Any): (String, Option[String]) = {
    val query = templateIsOutputCorrectForInput(testInputsMap, output)
    printQuery("\nQuery checkOnInputAndKnownOutput:\n" + query)
    solver.runSolver(query)
  }


  /**
    * Checks, if a program is correct for the particular test case.
    */
  def checkIsProgramCorrectForInput(s: Op,
                                    testModel: Map[String, Any]): (String, Option[String]) = {
    val query = templateIsProgramCorrectForInput(s, testModel)
    printQuery("\nQuery checkOnInputOnly:\n" + query)
    solver.runSolver(query)
  }


  /**
    * Simplifies a program by removing redundant elements and making trivial transformations
    * like replacing operations on constants with the result.
    */
  def simplifySolution(smtlib: String): Option[String] = {
    try {
      val query = templateSimplify(smtlib)
      println(s"Simplification query:\n$query")
      val res = solver.executeQuery(query)
      if (res.trim.startsWith("(error")) None else Some(res)
    }
    catch { case _: Throwable => None }
  }

  ///////////////////////////////////////////////////////


  /**
    * Saves state-related info and statistics in the collector.
    */
  override def reportData() {
    super.reportData()
    solver.reportData(coll)
  }

  def printQuery(s: String): Unit = {
    if (printQueries)
      println(s)
  }
}



























class StateCDGP2(sygusData: SygusProblemData)
                (implicit opt: Options, coll: Collector, rng: TRandom)
  extends StateSMTSolver(sygusData) {

  // Parameters
  val searchForSecondOutput = opt('searchForSecondOutput, true)
  val testsAbsDiff: Option[Int] = opt.getOptionInt("testsAbsDiff")
  val testsRatio: Double = opt('testsRatio, 1.0, (x: Double) => x >= 0.0 && x <= 1.0)
  val maxNewTestsPerIter: Int = opt('maxNewTestsPerIter, Int.MaxValue, (x: Int) => x > 0)

  // For statistic/diagnostic info
  var numRejectedCounterex = 0


  lazy val singleAnswerFormal: Boolean = isSingleAnswer(sygusData)


  def isSingleAnswer(sygusData: SygusProblemData): Boolean = {
    sygusData.singleInvocFormal && hasSingleAnswerForEveryInput(sygusData.problem).getOrElse(false)
  }

  /**
    * Creates a complete or incomplete test depending on the circumstances.
    */
  def createTestFromCounterex(model: Map[String, Any]): TestCase[I, O] = {
    if (!sygusData.singleInvocFormal || !sygusData.supportForAllTerms)
      IncompleteTestCase(model)
    else {
      try {
        val query = templateFindOutput(model)
        // printQuery("\nQuery findOutputForTestCase:\n" + query)
        val (dec, res) = solver.runSolver(query)
        if (dec == "sat") {
          val output: Option[Any] = GetValueParser(res.get).toMap.get(TemplateFindOutput.CORRECT_OUTPUT_VAR)
          if (singleAnswerFormal) // we have guarantees that the found output is the only correct one
            createCompleteTest(model, output)

          else {
            if (searchForSecondOutput) {
              // We are trying to find some other output which satisfies the constraints.
              // If we succeed, then test must be evaluated by solver in the future, hence the None
              // ain place of correct output.
              //
              // This can be especially useful if outputs for some parts of input space are undefined.
              // Undefinedness, which means that for such inputs every output is acceptable, makes the whole
              // problem to not have the single-answer property, even if otherwise all inputs have single-answer.
              val query2 = templateFindOutput(model, excludeValues = List(output.get))
              // printQuery("\nQuery findOutputForTestCase2:\n" + query2)
              val (dec2, res2) = solver.runSolver(query2)
              if (dec2 == "unsat")
                createCompleteTest(model, output) // single-output
              else
                IncompleteTestCase(model) // multiple-output
            }
            else
              IncompleteTestCase(model)   // assumed is multiple-output
          }
        }
        else if (dec == "unsat")
          throw new NoSolutionException(model.toString)
        else  // e.g. unknown
          IncompleteTestCase(model)
      }
      catch {
        case _: Throwable =>
          println(s"Exception during executing query or parsing result, returning test with no output! ")
          IncompleteTestCase(model)
      }
    }
  }

  def createTestFromFailedVerification(verOutput: String): Option[TestCase[I, O]] = {
    try {
      // NOTE: should map be used for this? Wouldn't Seq work better?
      val model = GetValueParser(verOutput).toMap // parse model returned by solver
      if (testsManager.tests.contains(model))
        None // this input already was used, there is no use in creating a test case for it
      else
        Some(createTestFromCounterex(model))
    } catch {
      case e: Throwable =>
        println(s"Error during creation of counterexample from: $verOutput\nOriginal message: " + e.getMessage)
        numRejectedCounterex += 1
        None
    }

  }

  /**
    * Saves state-related info and statistics in the collector.
    */
  override def reportData() {
    super.reportData()
    coll.set("cdgp.numRejectedCounterex", numRejectedCounterex)
  }
}


object StateCDGP2 {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): StateCDGP2 =
    StateCDGP2(LoadSygusBenchmark(benchmark))
  def apply(problem: SyGuS16)
           (implicit opt: Options, coll: Collector, rng: TRandom): StateCDGP2 = {
    new StateCDGP2(SygusProblemData(problem, opt('mixedSpecAllowed, true)))
  }
  def apply(problem: SygusProblemData)
           (implicit opt: Options, coll: Collector, rng: TRandom): StateCDGP2 = {
    new StateCDGP2(problem)
  }

  /**
    * Transforms a model returned by the solver to a mapping from synth-fun argument name
    * to value derived from the model.
    */
  def modelToSynthFunInputs(model: Map[String, Any], invocation: Seq[String],
                            sfArgNames: Seq[String]): Map[String, Any] =
    SygusUtils.renameVars(model, invocation, sfArgNames)
}









class StateGPR(sygusData: SygusProblemData)
              (implicit opt: Options, coll: Collector, rng: TRandom)
  extends StateCDGP2(sygusData) {

  // Parameters
  val gprRetryIfUndefined = opt('gprRetryIfUndefined, true)
  val GPRminInt: Int = opt('GPRminInt, -100)
  val GPRmaxInt: Int = opt('GPRmaxInt, 100)
  val GPRminDouble: Double = opt('GPRminDouble, 0.0)
  val GPRmaxDouble: Double = opt('GPRmaxDouble, 1.0)


  def createRandomTest(): Option[TestCase[I, O]] = {
    def sample(tpe: SortExpr): Any = tpe match {
      case IntSortExpr()  => GPRminInt + rng.nextInt(GPRmaxInt+1-GPRminInt)
      case RealSortExpr() => GPRminDouble + rng.nextDouble() * (GPRmaxDouble+1-GPRminDouble)
      case BoolSortExpr() => rng.nextBoolean()
      case _: Throwable   => throw new Exception(s"Trying to run GPR for unsupported type: ${tpe.name}.")
    }
    val model = sygusData.varDecls.map(v => (v.sym, sample(v.sortExpr))).toMap
    if (testsManager.tests.contains(model))
      createRandomTest() // try again
    else if (gprRetryIfUndefined) {
      // We will now check if for the input there exists an incorrect output.
      // This is necessary in case GPR generated a test with undefined answer.
      // Adding such a test is meaningless.
      val query = templateFindOutputNeg(model)
      val (dec, _) = solver.runSolver(query)
      if (dec == "unsat")
        createRandomTest() // try again
      else
        Some(createTestFromCounterex(model))
    }
    else
      Some(createTestFromCounterex(model))
  }


  /**
    * Saves state-related info and statistics in the collector.
    */
  override def reportData() {
    super.reportData()
  }
}











/**
  * Manages everything needed for the CDGP to run. Among other things, handles interaction
  * with the solver, and contains test manager. As argument given is the definition of the
  * SYGUS problem to be solved, and from it extracted are all important information such
  * as grammar and logic to be used.
  */
class CDGPState(val sygusData: SygusProblemData)
               (implicit opt: Options, coll: Collector, rng: TRandom) {
  // The types for input and output
  type I = Map[String, Any]
  type O = Any
  val testsManager = new TestsManagerCDGP[I, O](opt("logTestsHistory", false), opt("printTests", false))

  // Other parameters
  val GPRminInt: Int = opt('GPRminInt, -100)
  val GPRmaxInt: Int = opt('GPRmaxInt, 100)
  val GPRminDouble: Double = opt('GPRminDouble, 0.0)
  val GPRmaxDouble: Double = opt('GPRmaxDouble, 1.0)
  val timeout: Int = opt('solverTimeout, if (opt('solverType, "z3") == "z3") 5000 else 0)
  val silent = opt('silent, false)
  val searchForSecondOutput = opt('searchForSecondOutput, true)
  val gprRetryIfUndefined = opt('gprRetryIfUndefined, true)
  val printQueries = opt('printQueries, false)


  def invocations: Seq[Seq[String]] = sygusData.formalInvocations
  def synthTask: SygusSynthTask = sygusData.synthTask
  lazy val singleAnswerFormal: Boolean = sygusData.singleInvocFormal && hasSingleAnswerForEveryInput(sygusData.problem).getOrElse(false)

  // Initializing population of test cases
  testsManager.addNewTests(sygusData.testCasesConstrToTests)
  // testsManager.flushHelpers() // This is done elsewhere (at the beginning of evolution)

  // Creating solver manager
  private val solverPath = opt('solverPath)
  private val solverArgs = opt.getOption("solverArgs")
  private val moreSolverArgs = opt.getOption("moreSolverArgs", "")
  lazy val solver = new SolverManager(solverPath, solverArgs, moreSolverArgs, verbose=false)

  // Templates for solver queries
  lazy val templateVerification = new TemplateVerification(sygusData, timeout = timeout)
  lazy val templateIsOutputCorrectForInput = new TemplateIsOutputCorrectForInput(sygusData, timeout = timeout)
  lazy val templateIsProgramCorrectForInput = new TemplateIsProgramCorrectForInput(sygusData, timeout = timeout)
  lazy val templateFindOutput = new TemplateFindOutput(sygusData, timeout = timeout)
  lazy val templateFindOutputNeg = new TemplateFindOutput(sygusData, negateConstr = true, timeout = timeout)
  lazy val templateSimplify = new TemplateSimplify(sygusData, timeout = timeout)

  // For statistic/diagnostic info
  var numRejectedCounterex = 0

  printProblemInfo()

  def printProblemInfo() {
    println(s"(singleInvocationProperty ${sygusData.singleInvocFormal})")
    coll.set("cdgp.singleInvocationProperty", sygusData.singleInvocFormal)
    if (sygusData.singleInvocFormal) {
      // Checking for the single answer property has sense only if the problem
      // has single invocation property.
      println(s"(singleAnswerForEveryInput $singleAnswerFormal)")
      println(s"(supportForAllTerms ${sygusData.supportForAllTerms})")
      coll.set("cdgp.singleAnswerForEveryInput", singleAnswerFormal)
      coll.set("cdgp.supportForAllTerms", sygusData.supportForAllTerms)
    }
  }

//  /**
//    * Tests a program on the available tests and returns the vector of 0s (passed test)
//    * and 1s (failed test). Depending on the problem will either optimize by executing
//    * program directly on the tests, or will have to resort to a solver.
//    */
//  def evalOnTests(s: Op, tests: Seq[(I, Option[O])]): Seq[Int] = {
//    def handleException(test: (I, Option[O]), message: String) {
//      val msg = s"Error during evalutation of $s and test $test: $message"
//      coll.set("error_evalOnTests", msg)
//      println(msg)
//    }
//    for (test <- tests) yield {
//      try {
//        if (test._2.isDefined)
//          // User can define test cases for a problem, in which generally single-answer
//          // property does not hold. We will use domain for those cases, since it is more
//          // efficient.
//          evalOnTestsDomain(s, test)
//        else evalOnTestsSolver(s, test)
//      }
//      catch { case e: Throwable => handleException(test, e.getMessage); 1 }
//    }
//  }
//
//  /**
//    * Checks correctness of the program only for the given test.
//    * Tests here always have None as the answer, because in general there is no
//    * single answer for the problem being solved in 'solver' mode.
//    */
//  def evalOnTestsSolver(s: Op, test: (I, Option[O])): Int = {
//    val testModel: Map[String, Any] = test._1
//    val (dec, _) = checkIsProgramCorrectForInput(s, testModel)
//    if (dec == "sat") 0 else 1
//  }
//
//  /**
//    * Checks correctness of the program only for the given test.
//    * If test has a defined expected answer, then it is compared with the answer
//    * obtained by executing the program in the domain simulating semantics of SMT
//    * theory.
//    * If test don't have defined expected answer, then the program's output is verified
//    * by the solver for consistency with the specification. The test will be updated if
//    * this output is deemed consistent by the solver.
//    *
//    * Names of variables in test should be the same as those in the function's invocation.
//    * They will be renamed for those in the function's declaration.
//    */
//  def evalOnTestsDomain(s: Op, test: (I, Option[O])): Int = {
//    assert(test._2.isDefined, "Trying to evaluate using the domain a test without defined expected output.")
//    val testInput: Map[String, Any] = test._1
//    val testOutput: Option[Any] = test._2
//    val inputVector = synthTask.argNames.map(testInput(_))
//    val output = domain(s)(inputVector)
//    if (output.isEmpty)
//      1  // None means that recurrence depth was exceeded
//    else if (testOutput.isDefined) {
//      if (output.get == convertValue(testOutput.get))
//        0
//      else
//        1
//    }
//    else {
//      // Situation, when the test case has None as the expected output
//      // We don't allow such a situation
//      throw new Exception("Trying to domain-evaluate a test without defined correct answer!")
//
//      // The code below can be used to try to find the expected output for the test
//      //val (dec, _) = checkIsOutputCorrect(s, testInput, output.get)
//      //if (dec == "sat")
//      //  testsManager.updateTest((testInput, output))
//      //if (dec == "sat") 0 else 1
//    }
//  }

  def convertValue(value: Any): Any =
    value match {
      case str: String => Tools.convertToJavaString(str)
      case _ => value
    }

  /**
    * Transforms a model returned by the solver to a mapping from synth-fun argument name
    * to value derived from the model.
    */
  def modelToSynthFunInputs(model: Map[String, Any]): Map[String, Any] =
    CDGPState.modelToSynthFunInputs(model, invocations.head, synthTask.argNames)


  /**
    * Creates a complete test, i.e. a test containing both input and the only correct
    * output for that input.
    * @param model Valuation of synth-variables. Will be converted to synth-fun inputs
    *              in test case.
    * @param output An output of the test case computed by the solver.
    */
  def createCompleteTest(model: Map[String, Any], output: Option[O]): CompleteTestCase[I, O] = {
    val synthFunInputs = modelToSynthFunInputs(model)
    CompleteTestCase[I, O](synthFunInputs, output)
  }

  ///////  Interactions with the solver  ///////

  /**
    * Checks using SMT solver if the given problem has only one correct answer for
    * any input.
    */
  def hasSingleAnswerForEveryInput(problem: SyGuS16): Option[Boolean] = {
    if (sygusData.formalConstr.isEmpty)
      Some(false)
    else {
      val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(problem, sygusData, solverTimeout = timeout)
      // printQuery("\nQuery checkIfSingleAnswerForEveryInput:\n" + query)
      val (dec, model) = solver.runSolver(query)
      if (dec == "sat") {
        val values = GetValueParser(model.get)
        if (!silent) println("Example of multiple correct answers: " + values.mkString(" "))
        Some(false)
      }
      else if (dec == "unsat") Some(true)
      else None
    }
  }

  def verify(s: Op): (String, Option[String]) = {
    val query = templateVerification(s)
    printQuery("\nQuery verify:\n" + query)
    solver.runSolver(query)
  }

  def checkIsOutputCorrectForInput(s: Op,
                                   testInputsMap: Map[String, Any],
                                   output: Any): (String, Option[String]) = {
    val query = templateIsOutputCorrectForInput(testInputsMap, output)
    printQuery("\nQuery checkOnInputAndKnownOutput:\n" + query)
    solver.runSolver(query)
  }

  def checkIsProgramCorrectForInput(s: Op,
                                    testModel: Map[String, Any]): (String, Option[String]) = {
    val query = templateIsProgramCorrectForInput(s, testModel)
    printQuery("\nQuery checkOnInputOnly:\n" + query)
    solver.runSolver(query)
  }

  /**
    * Creates a complete or incomplete test depending on the circumstances.
    */
  def createTestFromCounterex(model: Map[String, Any]): TestCase[I, O] = {
    if (!sygusData.singleInvocFormal || !sygusData.supportForAllTerms)
      IncompleteTestCase(model)
    else {
      try {
        val query = templateFindOutput(model)
        // printQuery("\nQuery findOutputForTestCase:\n" + query)
        val (dec, res) = solver.runSolver(query)
        if (dec == "sat") {
          val output: Option[Any] = GetValueParser(res.get).toMap.get(TemplateFindOutput.CORRECT_OUTPUT_VAR)
          if (singleAnswerFormal) // we have guarantees that the found output is the only correct one
            createCompleteTest(model, output)

          else {
            if (searchForSecondOutput) {
              // We are trying to find some other output which satisfies the constraints.
              // If we succeed, then test must be evaluated by solver in the future, hence the None
              // ain place of correct output.
              //
              // This can be especially useful if outputs for some parts of input space are undefined.
              // Undefinedness, which means that for such inputs every output is acceptable, makes the whole
              // problem to not have the single-answer property, even if otherwise all inputs have single-answer.
              val query2 = templateFindOutput(model, excludeValues = List(output.get))
              // printQuery("\nQuery findOutputForTestCase2:\n" + query2)
              val (dec2, res2) = solver.runSolver(query2)
              if (dec2 == "unsat")
                createCompleteTest(model, output) // single-output
              else
                IncompleteTestCase(model) // multiple-output
            }
            else
              IncompleteTestCase(model)   // assumed is multiple-output
          }
        }
        else if (dec == "unsat")
          throw new NoSolutionException(model.toString)
        else  // e.g. unknown
          IncompleteTestCase(model)
      }
      catch {
        case _: Throwable =>
          println(s"Exception during executing query or parsing result, returning test with no output! ")
          IncompleteTestCase(model)
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




  def createTestFromFailedVerification(verOutput: String): Option[TestCase[I, O]] = {
    try {
      // NOTE: should map be used for this? Wouldn't Seq work better?
      val model = GetValueParser(verOutput).toMap // parse model returned by solver
      if (testsManager.tests.contains(model))
        None // this input already was used, there is no use in creating a test case for it
      else
        Some(createTestFromCounterex(model))
    } catch {
      case e: Throwable =>
        println(s"Error during creation of counterexample from: $verOutput\nOriginal message: " + e.getMessage)
        numRejectedCounterex += 1
        None
    }
  }

  def createRandomTest(): Option[TestCase[I, O]] = {
    def sample(tpe: SortExpr): Any = tpe match {
      case IntSortExpr()  => GPRminInt + rng.nextInt(GPRmaxInt+1-GPRminInt)
      case RealSortExpr() => GPRminDouble + rng.nextDouble() * (GPRmaxDouble+1-GPRminDouble)
      case BoolSortExpr() => rng.nextBoolean()
      case _: Throwable   => throw new Exception(s"Trying to run GPR for unsupported type: ${tpe.name}.")
    }
    val model = sygusData.varDecls.map(v => (v.sym, sample(v.sortExpr))).toMap
    if (testsManager.tests.contains(model))
      createRandomTest() // try again
    else if (gprRetryIfUndefined) {
      // We will now check if for the input there exists an incorrect output.
      // This is necessary in case GPR generated a test with undefined answer.
      // Adding such a test is meaningless.
      val query = templateFindOutputNeg(model)
      val (dec, _) = solver.runSolver(query)
      if (dec == "unsat")
        createRandomTest() // try again
      else
        Some(createTestFromCounterex(model))
    }
    else
      Some(createTestFromCounterex(model))
  }



//  /**
//    * A fitness function which assigns 0 to passed tests and 1 to failed tests.
//    */
//  val fitness: (Op) => (Boolean, Seq[Int]) =
//    method match {
//      case _ if sygusData.formalInvocations.isEmpty => fitnessOnlyTestCases
//      case "CDGP"     => fitnessCDGPGeneral
//      case "GPR"      => fitnessGPR
//    }
//
//  def fitnessNoVerification(s: Op): (Boolean, Seq[Int]) = {
//    (false, evalOnTests(s, testsManager.getTests()))
//  }
//
//  /**
//    * Fitness is computed on the test cases. No verification is performed.
//    * A solution passing all test cases is considered optimal.
//    */
//  def fitnessOnlyTestCases: Op => (Boolean, Seq[Int]) =
//    (s: Op) => {
//      val evalTests = evalOnTests(s, testsManager.getTests())
//      if (evalTests.sum == 0)
//        (true, evalTests)
//      else
//        (false, evalTests)
//    }
//
//  def doVerify(evalTests: Seq[Int]): Boolean = {
//    val numPassed = evalTests.count(_ == 0).asInstanceOf[Double]
//    if (testsAbsDiff.isDefined)
//      numPassed >= evalTests.size - testsAbsDiff.get
//    else
//      evalTests.isEmpty || (numPassed / evalTests.size) >= testsRatio
//  }
//
//  /** Fitness is always computed on the tests that were flushed. */
//  def fitnessCDGPGeneral: Op => (Boolean, Seq[Int]) =
//    (s: Op) => {
//      val evalTests = evalOnTests(s, testsManager.getTests())
//      // If the program passes the specified ratio of test cases, it will be verified
//      // and a counterexample will be produced (or program will be deemed correct).
//      // NOTE: if the program does not pass all test cases, then the probability is high
//      // that the produced counterexample will already be in the set of test cases.
//      if (!doVerify(evalTests))
//        (false, evalTests)
//      else {
//        val (decision, r) = verify(s)
//        if (decision == "unsat" && evalTests.sum == 0 && (!(sygusData.logic == "SLIA") || evalTests.nonEmpty))
//          (true, evalTests) // perfect program found; end of run
//        else if (decision == "sat") {
//          if (testsManager.newTests.size < maxNewTestsPerIter) {
//            val newTest = createTestFromFailedVerification(r.get)
//            if (newTest.isDefined)
//              testsManager.addNewTest(newTest.get)
//          }
//          (false, evalTests)
//        }
//        else {
//          // The 'unknown' or 'timeout' solver's decision. Program potentially may be the optimal
//          // solution, but solver is not able to verify this. We proceed by adding no new tests
//          // and treating the program as incorrect.
//          (false, evalTests)
//        }
//      }
//    }
//
//  def fitnessGPR: Op => (Boolean, Seq[Int]) = {
//    new Function1[Op, (Boolean, Seq[Int])] {
//      def allTestsPassed(evalTests: Seq[Int]): Boolean =
//        evalTests.count(_ == 0) == evalTests.size
//      def generateAndAddRandomTest(): Unit = {
//        if (testsManager.newTests.size < maxNewTestsPerIter) {
//          val newTest = createRandomTest()
//          if (newTest.isDefined)
//            testsManager.addNewTest(newTest.get)
//        }
//      }
//      def apply(s: Op): (Boolean, Seq[Int]) = {
//        val evalTests = evalOnTests(s, testsManager.getTests())
//        if (!doVerify(evalTests))
//          (false, evalTests)
//        else if (allTestsPassed(evalTests)) {
//          // program passes all tests - verify if it is correct
//          val (decision, _) = verify(s)
//          if (decision == "unsat" && evalTests.sum == 0 && (!(sygusData.logic == "SLIA") || evalTests.nonEmpty))
//            (true, evalTests)  // perfect program found; end of run
//          else {
//            generateAndAddRandomTest()  // program incorrect; generate random test
//            (false, evalTests)
//          }
//        }
//        else {  // program passes enough tests but not all - generate random counterexample
//          generateAndAddRandomTest()
//          (false, evalTests)
//        }
//      }
//    }
//  }

//  def updateEvalInt(s: (Op, FInt)): (Op, FInt) = {
//    val newFit = FInt(s._2.correct, s._2.value + evalOnTests(s._1, testsManager.newTests.toList).sum, s._1.size, testsManager.getNumberOfTests)
//    (s._1, newFit)
//  }
//  def updateEvalSeqInt(s: (Op, FSeqInt)): (Op, FSeqInt) =
//    (s._1, FSeqInt(s._2.correct, s._2.value ++ evalOnTests(s._1, testsManager.newTests.toList), s._1.size))

  def printQuery(s: String): Unit = {
    if (printQueries)
      println(s)
  }

  /**
    * Saves state-related info and statistics in the collector.
    */
  def reportData() {
    testsManager.reportData(coll)
    solver.reportData(coll)
    coll.set("cdgp.numRejectedCounterex", numRejectedCounterex)
  }
}


object CDGPState {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPState =
    CDGPState(LoadSygusBenchmark(benchmark))

  def apply(problem: SyGuS16)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPState = {
    new CDGPState(SygusProblemData(problem, opt('mixedSpecAllowed, true)))
  }

  def apply(problem: SygusProblemData)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPState = {
    new CDGPState(problem)
  }


  /**
    * Transforms a model returned by the solver to a mapping from synth-fun argument name
    * to value derived from the model.
    */
  def modelToSynthFunInputs(model: Map[String, Any], invocation: Seq[String],
                            sfArgNames: Seq[String]): Map[String, Any] =
    SygusUtils.renameVars(model, invocation, sfArgNames)
}

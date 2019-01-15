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
  val testsManager = new TestsManagerCDGP[I, O](opt("logTestsHistory", false), opt("printTests", false), opt("saveTests", false))
  val silent = opt('silent, false)

  // Initializing population of test cases
  testsManager.addNewTests(sygusData.testCasesConstrToTests)
  // testsManager.flushHelpers() // This is done elsewhere (at the beginning of evolution)

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

  def simplifySolution(smtlib: String): Option[String] = None

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
    StateCDGP.modelToSynthFunInputs(model, invocations.head, synthTask.argNames)


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
    * all possible inputs. Note: a contradictory program will get unsat on the query
    * and this function will also return true.
    */
  def hasSingleAnswerForEveryInput(problem: SyGuS16): Option[Boolean] = {
    if (sygusData.formalConstr.isEmpty)
      Some(false)
    else {
      val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(problem, sygusData, solverTimeout = timeout)
      printQuery("\nQuery hasSingleAnswerForEveryInput:\n" + query)
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
    printQuery("\nQuery checkIsOutputCorrect:\n" + query)
    solver.runSolver(query)
  }


  /**
    * Checks, if a program is correct for the particular test case.
    */
  def checkIsProgramCorrectForInput(s: Op,
                                    testModel: Map[String, Any]): (String, Option[String]) = {
    val query = templateIsProgramCorrectForInput(s, testModel)
    printQuery("\nQuery checkIsProgramCorrectForInput:\n" + query)
    solver.runSolver(query)
  }


  /**
    * Simplifies a program by removing redundant elements and making trivial transformations
    * like replacing operations on constants with the result.
    */
  override def simplifySolution(smtlib: String): Option[String] = {
    try {
      val query = templateSimplify(smtlib)
      printQuery(s"Simplification query:\n$query")
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









class StateCDGP(sygusData: SygusProblemData)
               (implicit opt: Options, coll: Collector, rng: TRandom)
  extends StateSMTSolver(sygusData) {

  // Parameters
  val searchForSecondOutput = opt('searchForSecondOutput, true)
  val testsAbsDiff: Option[Int] = opt.getOptionInt("testsAbsDiff")
  val testsRatio: Double = opt('testsRatio, 1.0, (x: Double) => x >= 0.0 && x <= 1.0)
  val maxNewTestsPerIter: Int = opt('maxNewTestsPerIter, Int.MaxValue, (x: Int) => x >= 0)

  // For statistic/diagnostic info
  var numRejectedCounterex = 0


  lazy val singleAnswerFormal: Boolean = isSingleAnswer(sygusData)

  if (!silent)
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
        case e: NoSolutionException => throw e
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
      case e: NoSolutionException => throw e
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


object StateCDGP {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): StateCDGP =
    StateCDGP(LoadSygusBenchmark(benchmark))
  def apply(problem: SyGuS16)
           (implicit opt: Options, coll: Collector, rng: TRandom): StateCDGP = {
    new StateCDGP(SygusProblemData(problem, opt('mixedSpecAllowed, true)))
  }
  def apply(problem: SygusProblemData)
           (implicit opt: Options, coll: Collector, rng: TRandom): StateCDGP = {
    new StateCDGP(problem)
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
  extends StateCDGP(sygusData) {

  // Parameters
  val gprRetryIfUndefined = opt('gprRetryIfUndefined, true)
  val GPRminInt: Int = opt('GPRminInt, -100)
  val GPRmaxInt: Int = opt('GPRmaxInt, 100, (x:Int) => x >= GPRminInt)
  val GPRminDouble: Double = opt('GPRminDouble, 0.0)
  val GPRmaxDouble: Double = opt('GPRmaxDouble, 1.0, (x:Double) => x >= GPRminDouble)

  def createRandomTest(): Option[TestCase[I, O]] = {
    def sample(tpe: SortExpr): Any = tpe match {
      case IntSortExpr()  => GPRminInt + rng.nextInt(GPRmaxInt+1-GPRminInt)
      case RealSortExpr() => GPRminDouble + rng.nextDouble() * (GPRmaxDouble-GPRminDouble)
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



object StateGPR {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): StateGPR =
    StateGPR(LoadSygusBenchmark(benchmark))
  def apply(problem: SyGuS16)
           (implicit opt: Options, coll: Collector, rng: TRandom): StateGPR = {
    new StateGPR(SygusProblemData(problem, opt('mixedSpecAllowed, true)))
  }
  def apply(problem: SygusProblemData)
           (implicit opt: Options, coll: Collector, rng: TRandom): StateGPR = {
    new StateGPR(problem)
  }
}

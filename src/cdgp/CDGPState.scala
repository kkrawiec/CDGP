package cdgp

import fuel.util.{Collector, Options, TRandom}
import swim.tree.Op
import sygus.VarDeclCmd
import sygus16.SyGuS16

import scala.collection.immutable.Map
import scala.collection.{Seq, mutable}



/**
  * Manages the set of test cases during evolution run.
  */
class TestsManagerCDGP[I,O](testsHistory: Boolean = true) {
  // Set of counterexamples collected along the run.
  // The Option is None if the desired output for a given input is not known yet.
  val tests: mutable.LinkedHashMap[I, Option[O]] = mutable.LinkedHashMap[I, Option[O]]()
  // Set of counterexamples collected from the current generation. To be reseted after each iteration.
  val newTests: mutable.Set[(I, Option[O])] = mutable.Set[(I, Option[O])]()

  // Stores the number of tests after each use of flushHelpers.
  val history: mutable.MutableList[Int] = mutable.MutableList[Int]()

  def getTests(): List[(I, Option[O])] = {
    tests.toList
  }
  def getNumberOfTests: Int = tests.size
  def getNumberOfKnownOutputs: Int = tests.values.count(_.isDefined)
  def getNumberOfUnknownOutputs: Int = tests.size - getNumberOfKnownOutputs

  def addNewTest(t: (I, Option[O])) {
    //println("** Trying to add new test: " + t)
    if (!tests.contains(t._1)) {
      //println("** ADDED")
      newTests.+=(t)
    }
  }
  def updateTest(t: (I, Option[O])) {
    //println("** Updated test: " + t)
    tests.put(t._1, t._2)
  }

  /**
    * Moves elements from newTests to a global tests pool, and prepares manager for the next iteration
    * by clearing newTests.
    */
  def flushHelpers() {
    for (test <- newTests)
      if (!tests.contains(test._1)) {
        tests.put(test._1, test._2)
      }
    if (testsHistory) history += tests.size
    newTests.clear
  }
}



/**
  * Manages everything needed for the CDGP to run. Among other things, handles interaction
  * with the solver, and contains test manager. As argument given is the definition of the
  * SYGUS problem to be solved, and from it extracted are all important information such
  * as grammar and logic to be used.
  */
class CDGPState(sygusProblem: SyGuS16)
               (implicit opt: Options, coll: Collector, rng: TRandom) {
  // The types for input and output
  type I = Map[String, Any]
  type O = Any
  val testsManager = new TestsManagerCDGP[I, O]()

  val method = opt('method, "CDGP")
  val searchAlg = opt('searchAlgorithm)
  assume(method == "CDGP" || method == "CDGPcons" || method == "GPR")
  assume(searchAlg == "GP" || searchAlg == "GPSteadyState" ||
         searchAlg == "Lexicase" || searchAlg == "LexicaseSteadyState")


  // Other parameters
  val GPRminInt = opt('GPRminInt, -100)
  val GPRmaxInt = opt('GPRmaxInt, 100)
  val CDGPoneTestPerIter = opt('CDGPoneTestPerIter, false)
  val GPRoneTestPerIter = opt('GPRoneTestPerIter, true)
  val silent = opt('silent, false)
  val sygusOutput = opt('sygusOutput, true)


  /**
    * Checks using SMT solver if the given problem has only one correct answer for
    * any input.
    */
  def hasSingleAnswerForEveryInput(problem: SyGuS16): Option[Boolean] = {
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(problem)
    // println("\nQuery checkIfSingleAnswerForEveryInput:\n" + query)
    val (dec, _) = solver.runSolver(query)
    if (dec == "sat") Some(false)
    else if (dec == "unsat") Some(true)
    else None
  }

  def getTestCasesMode(problem: SyGuS16): String = {
    val singleInvoc = SygusUtils.hasSingleInvocationProperty(sygusProblem)
    println(f"(singleInvocationProperty $singleInvoc)")
    coll.set("cdgp.singleInvocationProperty", singleInvoc)
    if (singleInvoc) {
      // Checking for the single answer has sense only if the problem
      // has single invocation property.
      val singleAnswer = hasSingleAnswerForEveryInput(sygusProblem)
      val supportForAllTerms = !SygusUtils.containsUnsupportedComplexTerms(sygusProblem)
      println(f"(singleAnswerForEveryInput ${singleAnswer.getOrElse("unknown")})")
      println(f"(supportForAllTerms $supportForAllTerms)")
      coll.set("cdgp.singleAnswerForEveryInput", singleAnswer.getOrElse("unknown"))
      coll.set("cdgp.supportForAllTerms", supportForAllTerms)
      if (singleAnswer.getOrElse(false) && supportForAllTerms)
        "gp"  // it may be considered to treat unknown singleAnswer as true, with the potential soundness loss of fitness
      else
        "solver"
    }
    else
      "solver"
  }

  /*
   * Depending on the properties of the problem, CDGPState will switch between using
   * GP domain and executing the solver for computing fitness.
   */
  val testCasesMode: String = getTestCasesMode(sygusProblem)
  assume(testCasesMode == "solver" || testCasesMode == "gp",
    "Possible values for --testCasesMode: 'solver', 'gp'.")
  val useDomainToComputeFitness: Boolean = testCasesMode == "gp"

  println(f"(testCasesMode $testCasesMode)")
  coll.set("cdgp.testCasesMode", testCasesMode)
  if (testCasesMode == "solver")
    println("WARNING: solver will be used to compute fitness. Expect major efficiency decrease" +
      " in comparison with GP test cases mode.")




  // Sygus parsing. TODO: relegate elsewhere
  val synthTasks = ExtractSynthesisTasks(sygusProblem)
  if (synthTasks.size > 1)
    throw new Exception("SKIPPING: Multiple synth-fun commands detected. Cannot handle such problems.")
  val synthTask: SygusSynthesisTask = synthTasks.head
  val invocations: Seq[Seq[String]] = SygusUtils.getSynthFunsInvocationsInfo(sygusProblem, synthTask.fname)
  //assume(invocations.size == 1, "To run test cases in gp mode synth-function in constraints must have" +
  //  " single invocation property and only single correct answer for any input.")
  val grammar = ExtractSygusGrammar(synthTask)

  private def fv = sygusProblem.cmds.collect { case v: VarDeclCmd => v }
  private val getValueCommand = f"(get-value (${fv.map(_.sym).mkString(" ")}))"


  // Creating solver manager
  private def solverPath = opt('solverPath)
  private def solverArgs = opt('solverArgs, "-in")
  lazy val solver = new SolverManager(solverPath, solverArgs, verbose=false)



  /**
    * Tests a program on the available tests and returns the vector of 0s (passed test)
    * and 1s (failed test). If the desired output is not known for a test, uses a solver
    * to determine it, and saves the obtained desired output.
    */
  def evalOnTests(s: Op, tests: Seq[(I, Option[O])]): Seq[Int] = {
    def handleException(test: (I, Option[O]), message: String) {
      val msg = f"Error during evalutation of $s and test $test: $message"
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
    val testInputsMap: Map[String, Any] = test._1
    val (dec, _) = checkOnInputOnly(s, testInputsMap)
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
    */
  def evalOnTestsDomain(s: Op, test: (I, Option[O])): Int = {
    val testInputsMap: Map[String, Any] = test._1
    val testOutput: Option[Any] = test._2
    /**
      * Solver requires renaming of the variables, so that it is able to handle cases
      * similar to the following:
      *   (synth-fun synthFun ((x Int)(y Int)) ... )
      *   (declare-var a Int)
      *   (declare-var b Int)
      *   (constraint (=> (= (- b a) (- c b)) (= (synthFun a b) 1)))
      *
      * This renaming is only needed for the execution of the program by the domain.
      */
    val inv: Seq[String] = invocations.head
    val namesMap = inv.zip(synthTask.argNames).toMap  // Map from names in function signature to names of variables used in function invocation
    val testInputsRenamed = inv.map{ s => (namesMap(s), testInputsMap(s)) }.toMap
    // TODO: implement domains other than LIA
    val output = LIA(s)(testInputsRenamed)

    if (testOutput.isDefined) {
      if (output == testOutput.get) 0 else 1
    }
    else {
      val (dec, _) = checkOnInputAndKnownOutput(s, testInputsMap, output)
      if (dec == "sat")
        testsManager.updateTest((testInputsMap, Some(output)))
      if (dec == "sat") 0 else 1
    }
  }


  def checkOnInputAndKnownOutput(s: Op,
                                 testInputsMap: Map[String, Any],
                                 output: Any): (String, Option[String]) = {
    val query = SMTLIBFormatter.checkOnInputAndKnownOutput(sygusProblem, testInputsMap, output,
                  opt('solverTimeout, 0))
    // println("\nQuery checkOnInputAndKnownOutput:\n" + query)
    solver.runSolver(query)
  }


  def checkOnInputOnly(s: Op,
                       testInputsMap: Map[String, Any]): (String, Option[String]) = {
    val query = SMTLIBFormatter.checkOnInput(sygusProblem, testInputsMap, s, opt('solverTimeout, 0))
    // println("\nQuery checkOnInputOnly:\n" + query)
    solver.runSolver(query)
  }


  def verify(s: Op): (String, Option[String]) = {
    val query = SMTLIBFormatter.verify(sygusProblem, s, opt('solverTimeout, 0))
    // println("\nQuery verify:\n" + query)
    solver.runSolver(query, getValueCommand)
  }

  def tryToFindOutputForTestCase(test: (I, Option[O])): (I, Option[O]) = {
    val query = SMTLIBFormatter.searchForCorrectOutput(sygusProblem, test._1, solverTimeout=opt('solverTimeout, 0))
    // println("\nQuery searchForCorrectOutput:\n" + query)
    try {
      val getValueCommand = f"(get-value (CorrectOutput))"
      val (dec, res) = solver.runSolver(query, getValueCommand)
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

  def createTestFromFailedVerification(verOutput: String): (Map[String, Any], Option[Any]) = {
    val counterExample = GetValueParser(verOutput) // returns the counterexample
    val testNoOutput = (counterExample.toMap, None) // for this test currently the correct answer is not known
    if (testCasesMode == "gp")
      tryToFindOutputForTestCase(testNoOutput)
    else
      testNoOutput
  }

  def createRandomTest(verOutput: String): (Map[String, Any], Option[Any]) = {
    // The only reason to call the parser here is to get the right argument names:
    val argNames = GetValueParser(verOutput).unzip._1
    val example = argNames.map(argName => (argName, GPRminInt + rng.nextInt(GPRmaxInt+1-GPRminInt)))
    val testNoOutput = (example.toMap, None) // for this test currently the correct answer is not known
    tryToFindOutputForTestCase(testNoOutput)
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
      case "CDGP"     => fitnessCDGP
      case "CDGPcons" => fitnessCDGPConservative
      case "GPR"      => fitnessGPR
    }

  def fitnessCDGP: Op => (Boolean, Seq[Int]) =
    new Function1[Op, (Boolean, Seq[Int])] {
      def apply(s: Op) = {
        val (decision, r) = verify(s)
        if (decision == "unsat") (true, testsManager.getTests().map(_ => -1)) // perfect program found; end of run
        else {
          if (!CDGPoneTestPerIter || testsManager.newTests.isEmpty) {
            val newTest = createTestFromFailedVerification(r.get)
            testsManager.addNewTest(newTest)
          }
          val failedTests = evalOnTests(s, testsManager.getTests())
          (false, failedTests)
        }
      }
    }

  def fitnessCDGPConservative: Op => (Boolean, Seq[Int]) =
    new Function1[Op, (Boolean, Seq[Int])] {
      def apply(s: Op) = {
        val failedTests = evalOnTests(s, testsManager.getTests())
        val numFailed = failedTests.count(_ == 1)
        // CDGP Conservative variant: If the program fails any tests, then don't apply verification to it,
        // as it is very likely that the found counterexample is already among the tests.
        if (numFailed > 0)
          (false, failedTests)
        else {
          val (decision, r) = verify(s)
          if (decision == "unsat") (true, testsManager.getTests().map(_ => -1)) // perfect program found; end of run
          else {
            if (!CDGPoneTestPerIter || testsManager.newTests.isEmpty) {
              val newTest = createTestFromFailedVerification(r.get)
              testsManager.addNewTest(newTest)
            }
            (false, failedTests)
          }
        }
      }
    }

  def fitnessGPR: Op => (Boolean, Seq[Int]) = {
    new Function1[Op, (Boolean, Seq[Int])] {
      def apply(s: Op) = {
        val failedTests = evalOnTests(s, testsManager.getTests())
        val numFailed = failedTests.count(_ == 1)
        if (numFailed > 0)
          (false, failedTests)
        else {
          val (decision, r) = verify(s)
          if (decision == "unsat") (true, testsManager.getTests().map(_ => -1)) // perfect program found; end of run
          else {
            if (!GPRoneTestPerIter || testsManager.newTests.isEmpty) {
              val newTest = createRandomTest(r.get)
              testsManager.addNewTest(newTest)
            }
            (false, failedTests)
          }
        }
      }
    }
  }

  def updateEvalInt(s: (Op, FInt)): (Op, FInt) = {
    val newFit = FInt(s._2.correct, s._2.value + evalOnTests(s._1, testsManager.newTests.toList).sum)
    (s._1, newFit)
  }
  def updateEvalSeqInt(s: (Op, FSeqInt)): (Op, FSeqInt) =
    (s._1, FSeqInt(s._2.correct, s._2.value ++ evalOnTests(s._1, testsManager.newTests.toList)))
}


object CDGPState {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPState =
    new CDGPState(LoadSygusBenchmark(benchmark))

  def apply(sygusProblem: SyGuS16)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPState =
    new CDGPState(sygusProblem)
}


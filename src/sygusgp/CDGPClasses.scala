package sygusgp

import fuel.core.StatePop
import fuel.func.{EACore, Evaluation, ParallelEval, SequentialEval}
import fuel.util.{Collector, Options, TRandom}
import swim.tree.Op
import sygus.VarDeclCmd
import sygus16.SyGuS16

import scala.collection.immutable.Map
import scala.collection.Seq

/**
  * Wrapper on any evolutionary algorithm which will apply counterexample
  * guided methods using the provided solver.
  * @param alg
  * @tparam S Type of solution representation object (e.g. String, Seq[Int]).
  * @tparam E Type of evaluation object (e.g. Int, Seq[Int]).
  */
//class CDGP[S, E](alg: EACore[S, E])(implicit opt: Options) {
//  val newAlg = new EACore[S, E](alg.moves, alg.evaluation, alg.stop){
//      override def iter = alg.iter
//    }
//}


/**
  * Manages the set of test cases during evolution run.
  */
class TestsManagerCDGP[I,O]() {
  // Set of counterexamples collected along the run.
  // The Option is None if the desired output for a given input is not known yet.
  val tests = scala.collection.mutable.LinkedHashMap[I, Option[O]]()
  // Set of counterexamples collected from the current generation. To be reseted after each iteration.
  val newTests = scala.collection.mutable.Set[(I, Option[O])]()

  def getTests(): List[(I, Option[O])] = {
    tests.toList
  }
  def getNumberOfKnownOutputs(): Int = tests.values.count(_.isDefined)

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
    * Moves elements from newTests to a global tests pool, and prepares manager for the next iteration.
    */
  def flushHelpers() {
    // Update the set of tests with the newly found ones:
    for (test <- newTests)
      if (!tests.contains(test._1)) {
        tests.put(test._1, test._2)
      }
    newTests.clear
  }
}


class CGDPFitness(sygusProblem: SyGuS16)(implicit opt: Options, coll: Collector, rng: TRandom) {
  // The types for input and output
  type I = Map[String, Any]
  type O = Any
  val testsManager = new TestsManagerCDGP[I, O]()

  // Other parameters
  val GPRminInt = opt('GPRminInt, -100)
  val GPRmaxInt = opt('GPRmaxInt, 100)
  val CDGPoneTestPerIter = opt('CDGPoneTestPerIter, false)
  val GPRoneTestPerIter = opt('GPRoneTestPerIter, true)
  val silent = opt('silent, false)
  val sygusOutput = opt('sygusOutput, true)

  // Sygus parsing. TODO: relegate elsewhere
  val synthTasks = ExtractSynthesisTasks(sygusProblem)
  if (synthTasks.size > 1)
    throw new Exception("SKIPPING: Multiple synth-fun commands detected. Cannot handle such problems.")
  val synthTask = synthTasks.head
  val grammar = ExtractSygusGrammar(synthTask)

  val fv = sygusProblem.cmds.collect { case v: VarDeclCmd => v }
  val getValueCommand = f"(get-value (${fv.map(_.sym).mkString(" ")}))"


  // Creating solver manager
  val solverPath = opt('solverPath)
  val solverArgs = opt('solverArgs, "-in")
  val solver = new SolverManager(solverPath, solverArgs, verbose=false)

  val searchAlg = opt('searchAlgorithm, "")
  assume(searchAlg == "GP" || searchAlg == "GPSteadyState" ||
         searchAlg == "Lexicase" || searchAlg == "LexicaseSteadyState")


  /**
    * Tests a program on the available tests and returns the vector of 0s (passed test) and 1s (failed test)
    * If the desired output is not known for a test, uses a solver to determine it, and adds the obtained
    * desired output to testsWithFoundOutputs
    *
    */
  def evalOnTests(s: Op, tests: Seq[(I, Option[O])]): Seq[Int] = {
    for (test <- tests) yield {
      val testInputsMap = test._1
      val testOutput = test._2
      // Solver requires proper renaming of variables
      val cexampleRenamed = synthTask.argNames.zip(testInputsMap.unzip._2).toMap
      try {
        val output = LIA(s)(cexampleRenamed)
        // If the desired output for this test is already known, simply compare
        if (testOutput.isDefined) {
          if (output == testOutput.get) 0 else 1
        } else {
          // If the desired output is not known yet, use the solver:
          val checkOnTestCmd = SMTLIBFormatter.checkOnInput(sygusProblem, testInputsMap, output, solverTimeout=opt('solverTimeout, 0))
          println("\ncheckOnTestCmd:\n" + checkOnTestCmd)
          val (decision, outputData) = solver.runSolver(checkOnTestCmd)
          // If the program passed the test, we know the desired output and can update
          // the set of tests
          if (decision == "sat") testsManager.updateTest((testInputsMap, Some(output)))
          if (decision == "sat") 0 else 1
        }
      }
      catch {
        case e: Throwable => println(f"Error during evalutation of $s and test $test: ${e.getMessage}"); 1
      }
    }
  }


  def verify(s: Op): (String, Option[String]) = {
    val verProblemCmds = SMTLIBFormatter.verify(sygusProblem, s, solverTimeout=opt('solverTimeout, 0))
    solver.runSolver(verProblemCmds, getValueCommand)
  }

  def evaluateOnAllTests(s: Op): Boolean = {
    val failedTests = evalOnTests(s, testsManager.getTests())
    failedTests.count(_ == 1) == 0
  }

  def tryToFindOutputForTestCase(test: (I, Option[O])): (I, Option[O]) = {
    val cmd: String = SMTLIBFormatter.searchForCorrectOutput(sygusProblem, test._1, solverTimeout=opt('solverTimeout, 0))
    //println("\nSearch cmd:\n" + cmd)
    try {
      val getValueCommand = f"(get-value (CorrectOutput))"
      val (decision, res) = solver.runSolver(cmd, getValueCommand)
      // println("Solver res: " + res)
      if (decision == "sat") {
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
    println("verOutput:")
    println(verOutput)
    val counterExample = GetValueParser(verOutput) // returns the counterexample
    val testNoOutput = (counterExample.toMap, None) // for this test currently the correct answer is not known
    tryToFindOutputForTestCase(testNoOutput)
  }

  def createRandomTest(verOutput: String): (Map[String, Any], Option[Any]) = {
    // The only reason to call the parser here is to get the right argument names:
    val argNames = GetValueParser(verOutput).unzip._1
    val example = argNames.map(argName => (argName, GPRminInt + rng.nextInt(GPRmaxInt+1-GPRminInt)))
    val testNoOutput = (example.toMap, None) // for this test currently the correct answer is not known
    tryToFindOutputForTestCase(testNoOutput)
  }

  def fitnessCDGP: Op => Either[Op, Seq[Int]] =
    new Function1[Op, Either[Op, Seq[Int]]] {
      def apply(s: Op) = {
        val (decision, r) = verify(s)
        if (decision == "unsat") Left(s) // perfect program found; end of run
        else {
          if (!CDGPoneTestPerIter || testsManager.newTests.isEmpty) {
            val newTest = createTestFromFailedVerification(r.get)
            testsManager.addNewTest(newTest)
          }
          val failedTests = evalOnTests(s, testsManager.getTests())
          Right(failedTests)
        }
      }
    }

  def fitnessCDGPConservative: Op => Either[Op, Seq[Int]] =
    new Function1[Op, Either[Op, Seq[Int]]] {
      def apply(s: Op) = {
        val failedTests = evalOnTests(s, testsManager.getTests())
        val numFailed = failedTests.count(_ == 1)
        // CDGP Conservative variant: If the program fails any tests, then don't apply verification to it,
        // as it is very likely that the found counterexample is already among the tests.
        if (numFailed > 0)
          Right(failedTests)
        else {
          val (decision, r) = verify(s)
          if (decision == "unsat") Left(s) // perfect program found; end of run
          else {
            if (!CDGPoneTestPerIter || testsManager.newTests.isEmpty) {
              val newTest = createTestFromFailedVerification(r.get)
              testsManager.addNewTest(newTest)
            }
            Right(failedTests)
          }
        }
      }
    }

  def fitnessGPR(s: Op): Either[Op, Seq[Int]] = {
    val failedTests = evalOnTests(s, testsManager.getTests())
    val numFailed = failedTests.count(_ == 1)
    if (numFailed > 0)
      Right(failedTests)
    else {
      val (decision, r) = verify(s)
      if (decision == "unsat") Left(s) // perfect program found; end of run
      else {
        if (!GPRoneTestPerIter || testsManager.newTests.isEmpty) {
          val newTest = createRandomTest(r.get)
          testsManager.addNewTest(newTest)
        }
        Right(failedTests)
      }
    }
  }

  def evalInt[S](fitness: S=>Either[S, Seq[Int]]): S => Int = {
    (s: S) => {
      val r = fitness(s)
      if (r.isLeft) -1 // perfect program found; end of run
      else r.right.get.sum
    }
  }

  def evalSeqInt[S](fitness: S=>Either[S, Seq[Int]]): S => Seq[Int] = {
    (s: S) => {
      val r = fitness(s)
      if (r.isLeft) 0.until(testsManager.tests.size).map(_ => -1) // perfect program found; end of run
      else r.right.get
    }
  }

//  def eval[S]: S => E = searchAlg match {
//    case "GP" | "GPSteadyState" => evalInt
//    case "Lexicase" | "LexicaseSteadyState" => evalSeqInt
//  }
}


class CDGPEvaluation[S, E](testsManager: TestsManagerCDGP[Map[String, Any], Any],
                           fitness: Op => Either[Op, Seq[Int]],
                           eval: S => E)
                          (implicit opt: Options, coll: Collector, rng: TRandom)
      extends Evaluation[S, E]() {
  val silent = opt('silent, true)

  def evaluate: Evaluation[S, E] = if (opt('parEval, true)) ParallelEval(eval) else SequentialEval(eval)

  def cdgpEvaluate: StatePop[S] => StatePop[(S, E)] =
    evaluate.andThen(updateTestsManager)

  override def apply(s: StatePop[S]): StatePop[(S, E)] = cdgpEvaluate(s)

  def updateTestsManager(s: StatePop[(S, E)]): StatePop[(S, E)] = {
    val numNew = testsManager.newTests.size
    testsManager.flushHelpers()  // resets newTests
    if (!silent) {
      val numKnown = testsManager.tests.values.count(_.isDefined)
      println(f"Tests: found: ${numNew}  total: ${testsManager.tests.size}  known outputs: $numKnown")
    }
    s
  }
}


class CDGPEvaluationLexicase[S, E]() {

}
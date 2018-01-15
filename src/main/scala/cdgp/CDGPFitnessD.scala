package cdgp

import fuel.util.{Collector, Options, TRandom}
import swim.tree.Op
import sygus16.SyGuS16


/**
  * CDGP fitness utils for problems for which tests are either passed or not.
  */
class CDGPFitnessD(val state: CDGPState)
                  (implicit opt: Options, coll: Collector) {
  // The types for input and output
  type I = Map[String, Any]
  type O = Any

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
    val (dec, _) = state.checkIsProgramCorrectForInput(s, testModel)
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
    assert(test._2.isDefined, "Trying to evaluate using the domain a test without defined expected output.")
    val testInput: Map[String, Any] = test._1
    val testOutput: Option[Any] = test._2
    val inputVector = state.synthTask.argNames.map(testInput(_))
    val output = state.domain(s)(inputVector)
    if (output.isEmpty)
      1  // None means that recurrence depth was exceeded
    else if (testOutput.isDefined) {
      if (output.get == state.convertValue(testOutput.get))
        0
      else
        1
    }
    else {
      // Situation, when the test case has None as the expected output
      // We don't allow such a situation
      throw new Exception("Trying to domain-evaluate a test without defined correct answer!")

      // The code below can be used to try to find the expected output for the test
      //val (dec, _) = checkIsOutputCorrectForInput(s, testInput, output.get)
      //if (dec == "sat")
      //  testsManager.updateTest((testInput, output))
      //if (dec == "sat") 0 else 1
    }
  }


  /**
    * A fitness function which assigns 0 to passed tests and 1 to failed tests.
    */
  val fitness: (Op) => (Boolean, Seq[Int]) =
    state.method match {
      case _ if state.sygusData.formalInvocations.isEmpty => fitnessOnlyTestCases
      case "CDGP"     => fitnessCDGPGeneral
      case "GPR"      => fitnessGPR
    }

  def fitnessNoVerification(s: Op): (Boolean, Seq[Int]) = {
    (false, evalOnTests(s, state.testsManager.getTests()))
  }

  /**
    * Fitness is computed on the test cases. No verification is performed.
    * A solution passing all test cases is considered optimal.
    */
  def fitnessOnlyTestCases: Op => (Boolean, Seq[Int]) =
    (s: Op) => {
      val evalTests = evalOnTests(s, state.testsManager.getTests())
      if (evalTests.sum == 0)
        (true, evalTests)
      else
        (false, evalTests)
    }

  def doVerify(evalTests: Seq[Int]): Boolean = {
    val numPassed = evalTests.count(_ == 0).asInstanceOf[Double]
    if (state.testsAbsDiff.isDefined)
      numPassed >= evalTests.size - state.testsAbsDiff.get
    else
      evalTests.isEmpty || (numPassed / evalTests.size) >= state.testsRatio
  }

  /** Fitness is always computed on the tests that were flushed. */
  def fitnessCDGPGeneral: Op => (Boolean, Seq[Int]) =
    (s: Op) => {
      val evalTests = evalOnTests(s, state.testsManager.getTests())
      // If the program passes the specified ratio of test cases, it will be verified
      // and a counterexample will be produced (or program will be deemed correct).
      // NOTE: if the program does not pass all test cases, then the probability is high
      // that the produced counterexample will already be in the set of test cases.
      if (!doVerify(evalTests))
        (false, evalTests)
      else {
        val (decision, r) = state.verify(s)
        if (decision == "unsat" && evalTests.sum == 0 && (!(state.sygusData.logic == "SLIA") || evalTests.nonEmpty))
          (true, evalTests) // perfect program found; end of run
        else if (decision == "sat") {
          if (state.testsManager.newTests.size < state.maxNewTestsPerIter) {
            val newTest = state.createTestFromFailedVerification(r.get)
            if (newTest.isDefined)
              state.testsManager.addNewTest(newTest.get)
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

  def fitnessGPR: Op => (Boolean, Seq[Int]) = {
    new Function1[Op, (Boolean, Seq[Int])] {
      def allTestsPassed(evalTests: Seq[Int]): Boolean =
        evalTests.count(_ == 0) == evalTests.size
      def generateAndAddRandomTest(): Unit = {
        if (state.testsManager.newTests.size < state.maxNewTestsPerIter) {
          val newTest = state.createRandomTest()
          if (newTest.isDefined)
            state.testsManager.addNewTest(newTest.get)
        }
      }
      def apply(s: Op): (Boolean, Seq[Int]) = {
        val evalTests = evalOnTests(s, state.testsManager.getTests())
        if (!doVerify(evalTests))
          (false, evalTests)
        else if (allTestsPassed(evalTests)) {
          // program passes all tests - verify if it is correct
          val (decision, _) = state.verify(s)
          if (decision == "unsat" && evalTests.sum == 0 && (!(state.sygusData.logic == "SLIA") || evalTests.nonEmpty))
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
    val newFit = FInt(s._2.correct, s._2.value + evalOnTests(s._1, state.testsManager.newTests.toList).sum, s._1.size, state.testsManager.getNumberOfTests)
    (s._1, newFit)
  }
  def updateEvalSeqInt(s: (Op, FSeqInt)): (Op, FSeqInt) =
    (s._1, FSeqInt(s._2.correct, s._2.value ++ evalOnTests(s._1, state.testsManager.newTests.toList), s._1.size))
}


object CDGPFitnessD {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPFitnessD =
    apply(LoadSygusBenchmark(benchmark))

  def apply(sygusProblem: SyGuS16)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPFitnessD =
    apply(new CDGPState(sygusProblem))

  def apply(state: CDGPState)
           (implicit opt: Options, coll: Collector): CDGPFitnessD =
    new CDGPFitnessD(state)
}
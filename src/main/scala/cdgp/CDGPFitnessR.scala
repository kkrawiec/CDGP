package cdgp

import fuel.util.{Collector, Options, TRandom}
import swim.tree.Op
import sygus16.SyGuS16


/**
  * CDGP fitness utils for regression problems. The main difference is in
  * the fitness, which here has a continuous value.
  */
class CDGPFitnessR(val state: CDGPState)
                  (implicit opt: Options, coll: Collector) {
  // The types for input and output
  type I = Map[String, Any]
  type O = Any

  val eps: Double = opt.paramDouble('eps, 0.0001)

  /**
    * Tests a program on the available tests and returns the vector of 0s (passed test)
    * and 1s (failed test). Depending on the problem will either optimize by executing
    * program directly on the tests, or will have to resort to a solver.
    */
  def evalOnTests(s: Op, tests: Seq[(I, Option[O])]): Seq[Double] = {
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
  def evalOnTestsSolver(s: Op, test: (I, Option[O])): Double = {
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
  def evalOnTestsDomain(s: Op, test: (I, Option[O])): Double = {
    assert(test._2.isDefined, "Trying to evaluate using the domain a test without defined expected output.")
    val testInput: Map[String, Any] = test._1
    val testOutput: Option[Any] = test._2
    val inputVector = state.synthTask.argNames.map(testInput(_))
    val output = state.domain(s)(inputVector)
    if (output.isEmpty)
      Double.MaxValue   // None means that recurrence depth was exceeded
    else if (testOutput.isDefined)
      output.get.asInstanceOf[Double] - testOutput.get.asInstanceOf[Double]
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
  val fitness: (Op) => (Boolean, Seq[Double]) =
    state.method match {
      case _ => fitnessOnlyTestCases
    }


  def fitnessNoVerification(s: Op): (Boolean, Seq[Double]) = {
    (false, evalOnTests(s, state.testsManager.getTests()))
  }

  /**
    * Fitness is computed on the test cases. No verification is performed.
    * A solution passing all test cases is considered optimal.
    */
  def fitnessOnlyTestCases: Op => (Boolean, Seq[Double]) =
    (s: Op) => {
      val evalTests = evalOnTests(s, state.testsManager.getTests())
      if (evalTests.sum <= eps)
        (true, evalTests)
      else
        (false, evalTests)
    }


//  def updateEvalDouble(s: (Op, FDouble)): (Op, FDouble) = {
//    val newFit = FDouble(s._2.correct, s._2.value + evalOnTests(s._1, testsManager.newTests.toList).sum, s._1.size, testsManager.getNumberOfTests)
//    (s._1, newFit)
//  }
  def updateEvalSeqDouble(s: (Op, FSeqDouble)): (Op, FSeqDouble) =
    (s._1, FSeqDouble(s._2.correct, s._2.value ++ evalOnTests(s._1, state.testsManager.newTests.toList), s._1.size))
}


object CDGPFitnessR {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPFitnessR =
    apply(LoadSygusBenchmark(benchmark))

  def apply(sygusProblem: SyGuS16)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPFitnessR =
    apply(new CDGPState(sygusProblem))

  def apply(state: CDGPState)
           (implicit opt: Options, coll: Collector): CDGPFitnessR =
    new CDGPFitnessR(state)
}
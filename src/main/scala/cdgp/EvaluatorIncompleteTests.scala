package cdgp

import fuel.util.{Collector, Options}
import swim.tree.Op


/**
  * Evaluator of incomplete tests.
  */
class EvaluatorIncompleteTests[EVecEl](val state: StateCDGP,
                                       val binaryTestPassValue: EVecEl,
                                       val binaryTestFailValue: EVecEl)
                                      (implicit coll: Collector) {
  // The types for input and output
  type I = Map[String, Any]
  type O = Any

  def apply(s: Op, tests: Seq[(I, Option[O])]): Seq[EVecEl] = {
    tests.map(evalTest(s, _))
  }

  protected def handleEvalException(test: (I, Option[O]), s: Op, message: String) {
    val msg = s"Error during evaluation of $s and test $test: $message"
    coll.set("error_evalOnTests", msg)
    println(msg)
  }

  /**
    * Checks correctness of the program for the given test.
    * Tests here should always have None as the answer.
    */
  def evalTest(s: Op, test: (I, Option[O])): EVecEl = {
    try {
      assert(test._2.isEmpty, "Solver should not be used to evaluate complete tests.")
      val testInput: Map[String, Any] = test._1
      val (dec, _) = state.checkIsProgramCorrectForInput(s, testInput)
      if (dec == "sat") binaryTestPassValue else binaryTestFailValue
    }
    catch {
      case e: Throwable => handleEvalException(test, s, e.getMessage); binaryTestFailValue
    }
  }
}


object EvaluatorIncompleteTestsDiscrete {
  def apply(state: StateCDGP)
           (implicit opt: Options, coll: Collector): EvaluatorIncompleteTests[Int] = {
    new EvaluatorIncompleteTests[Int](state, 0, 1)
  }
}


object EvaluatorIncompleteTestsContinuous {
  def apply(state: StateCDGP)
           (implicit opt: Options, coll: Collector): EvaluatorIncompleteTests[Double] = {
    new EvaluatorIncompleteTests[Double](state, 0.0, 1.0)
  }
}
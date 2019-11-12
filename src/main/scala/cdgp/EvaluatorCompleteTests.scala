package cdgp

import fuel.util.Collector
import swim.RecursiveDomain
import swim.tree.Op


abstract class CompleteTestsEvaluator[EVecEl](val domain: RecursiveDomain[Any, Any],
                                              val state: State)
                                             (implicit coll: Collector) {
  // The types for input and output
  type I = Map[String, Any]
  type O = Any

  def apply(s: Op, tests: Seq[(I, Option[O])]): Seq[EVecEl] = {
    tests.map(evalTest(s, _))
  }

  /**
    * Checks correctness of the program only for the given test.
    *
    * Names of variables in test should be the same as those in the function's invocation.
    * They will be renamed for those in the function's declaration.
    */
  def evalTest(s: Op, test: (I, Option[O])): EVecEl

  protected def handleEvalException(test: (I, Option[O]), s: Op, message: String) {
    val msg = s"Error during evaluation of $s and test $test: $message"
    coll.set("error_evalOnTests", msg)
    println(msg)
  }
}


object EvaluatorCompleteTests {
  /**
    * Creates a domain, which is used for the execution of programs.
    */
  def getDomain(logic: String, state: State, recDepthLimit: Int = 1000): RecursiveDomain[Any, Any] = logic match {
    case "SLIA" | "NIA" | "LIA" | "QF_NIA" | "QF_LIA" | "S" | "QF_S" | "ALL" =>
      DomainSLIA(state.synthTask.argNames, Symbol(state.synthTask.fname), recDepthLimit)
    case "NRA" | "LRA" | "QF_NRA" | "QF_LRA"=>
      DomainReals(state.synthTask.argNames, Symbol(state.synthTask.fname), recDepthLimit)
    case _ =>
      throw new Exception(s"Trying to create domain for the unsupported logic: $logic")
  }
}


class CompleteTestsEvaluatorDiscrete(domain: RecursiveDomain[Any, Any],
                                     state: State)
                                    (implicit coll: Collector)
  extends CompleteTestsEvaluator[Int](domain, state) {
  /**
    * Checks correctness of the program only for the given test.
    *
    * Names of variables in test should be the same as those in the function's invocation.
    * They will be renamed for those in the function's declaration.
    */
  override def evalTest(s: Op, test: (I, Option[O])): Int = {
    assert(test._2.isDefined, "Trying to evaluate using the domain a test without defined expected output.")
    try {
      val testInput: Map[String, Any] = test._1
      val testOutput: Option[Any] = test._2
      val inputVector = state.synthTask.argNames.map(testInput(_))
      val output = domain(s)(inputVector)
      if (output.isEmpty)
        1 // None means that recurrence depth was exceeded
      else if (output.get == state.convertValue(testOutput.get))
        0 // output was correct
      else
        1 // output was incorrect
    }
    catch {
      case _: ExceptionIncorrectOperation => 1
      case e: Throwable => handleEvalException(test, s, e.getMessage); 1
    }
  }
}



class CompleteTestsEvaluatorContinuous(domain: RecursiveDomain[Any, Any],
                                       state: State)
                                      (implicit coll: Collector)
  extends CompleteTestsEvaluator[Double](domain, state) {
  /**
    * Checks correctness of the program only for the given test.
    * The expected output is compared with the answer obtained by executing the
    * program in the domain simulating the semantics of appropriate SMT theory.
    *
    * Names of variables in the test should be the same as those in the function's invocation.
    * They will be renamed for those in the function's declaration.
    */
  override def evalTest(s: Op, test: (I, Option[O])): Double = {
    assert(test._2.isDefined, "Trying to domain-evaluate using a test without defined expected output.")
    try {
      val testInput: Map[String, Any] = test._1
      val testOutput: Option[Any] = test._2
      val inputVector = state.synthTask.argNames.map(testInput(_))
      val output = domain(s)(inputVector)
      if (output.isEmpty)
        Double.MaxValue // Recurrence depth was exceeded
      else
        math.abs(output.get.asInstanceOf[Double] - testOutput.get.asInstanceOf[Double])
    }
    catch {
      case _: ExceptionIncorrectOperation => Double.PositiveInfinity
      case e: Throwable =>
        handleEvalException(test, s, e.getMessage)
        Double.PositiveInfinity
    }
  }
}
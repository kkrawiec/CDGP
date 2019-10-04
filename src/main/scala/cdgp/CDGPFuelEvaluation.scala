package cdgp

import fuel.core.StatePop
import fuel.func.{Evaluation, ParallelEval, SequentialEval}
import fuel.util.{Collector, Options, TRandom}



/**
  * CDGPEvaluation performs evaluation in the spirit of Counterexample-Driven Genetic Programming
  * (see the paper: I.Błądek, K.Krawiec, J.Swan, "Counterexample-Driven Genetic Programming", GECCO'17).
  *
  * GP starts with an empty test set and a formal specification of the correct program. In the most
  * conservative variant only individual passing all test cases is verified for consistency with
  * the specification by the SMT solver. If such a verification fails, solver returns a counterexample.
  * This counterexample is then added to the test set and the process continues.
  *
  * @param eval Function from a solution to its evaluation.
  * @param opt Options.
  * @param coll Collector for saving results and stats.
  * @param rng Pseudorandom generator.
  * @tparam S Type of solution representation object (e.g. String, Seq[Int]).
  * @tparam E Type of evaluation object (e.g. Int, Seq[Int]).
  */
class CDGPFuelEvaluation[S, E](val eval: EvalFunction[S, E])
                              (implicit opt: Options, coll: Collector, rng: TRandom)
  extends Evaluation[S, E] {
  private val silent = opt('silent, false)
  def state: State = eval.state
  def evaluatePopulation: Evaluation[S, E] = if (opt('parEval, false)) ParallelEval(eval) else SequentialEval(eval)

  override def apply(s: StatePop[S]): StatePop[(S, E)] = cdgpEvaluate(s)

  def cdgpEvaluate: StatePop[S] => StatePop[(S, E)] =
    updateTestsManager andThen evaluatePopulation

  def updateTestsManager[T]: StatePop[T] => StatePop[T] =
    (s: StatePop[T]) => {
      val numNew = eval.state.testsManager.newTests.size
      eval.state.testsManager.flushHelpers()  // adds newTests to all tests; resets newTests
      if (!silent) {
        val numKnown = eval.state.testsManager.getNumberOfKnownOutputs
        println(s"Tests: found: $numNew  total: ${eval.state.testsManager.tests.size}  known outputs: $numKnown")
        // println(state.testsManager.tests.mkString("", "\n", "\n"))
      }
      s
    }
}



/**
  * A modified version of CDGPFuelEvaluation which is specialized in accommodating steady state GP.
  * In steady state evaluated are, beside initial population, only single solutions selected in the
  * given iteration. In CDGP the number of test cases can increase, so updatePopulationEvalsAndTests
  * must be invoked after every creation of a new individual.
  *
  * @param eval Function from a solution to its evaluation. It is also dependent on the cdgpState,
  *             but because of many possible variants it should be created outside.
  * @param updateEval CDGP may add a new test after evaluation of a selected individual.
  *                   This means that evaluations of all individuals in the population need
  *                   to be updated. This is handled by this function.
  * @param opt Options.
  * @param coll Collector for saving results and stats.
  * @param rng Pseudorandom generator.
  * @tparam S Type of solution representation object (e.g. String, Seq[Int]).
  * @tparam E Type of evaluation object (e.g. Int, Seq[Int]).
  */
class CDGPFuelEvaluationSteadyState[S, E](eval: EvalFunction[S, E],
                                          updateEval: ((S, E)) => (S, E))
                                         (implicit opt: Options, coll: Collector, rng: TRandom)
  extends CDGPFuelEvaluation[S, E](eval) {

  override def cdgpEvaluate: StatePop[S] => StatePop[(S, E)] =
    updateTestsManager andThen evaluatePopulation andThen updatePopulationEvalsAndTests

  /**
    * 1) For every program in the population adds to its evaluation an evaluation on the
    * newly generated tests.
    * 2) Calls updateTestsManager so that the set of newly collected tests is reset.
    */
  def updatePopulationEvalsAndTests(s: StatePop[(S, E)]): StatePop[(S, E)] = {
    val s2 =
      if (eval.state.testsManager.newTests.nonEmpty)
        StatePop(s.map{ case (op, e) => updateEval(op, e) })
      else s
    updateTestsManager(s2)
  }
}
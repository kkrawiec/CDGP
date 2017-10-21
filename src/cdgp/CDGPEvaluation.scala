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
  * @param state Object handling the state of the CDGP.
  * @param eval Function from a solution to its evaluation. It is also dependent on the cdgpState,
  *             but because of many possible variants it should be created outside.
  * @param opt Options.
  * @param coll Collector for saving results and stats.
  * @param rng Pseudorandom generator.
  * @tparam S Type of solution representation object (e.g. String, Seq[Int]).
  * @tparam E Type of evaluation object (e.g. Int, Seq[Int]).
  */
class CDGPEvaluation[S, E](val state: CDGPState,
                           val eval: S => E)
                          (implicit opt: Options, coll: Collector, rng: TRandom)
  extends Evaluation[S, E] {
  private val silent = opt('silent, false)
  def evaluate: Evaluation[S, E] = if (opt('parEval, false)) ParallelEval(eval) else SequentialEval(eval)

  override def apply(s: StatePop[S]): StatePop[(S, E)] = cdgpEvaluate(s)

  def cdgpEvaluate: StatePop[S] => StatePop[(S, E)] =
    updateTestsManager andThen evaluate

  def updateTestsManager[T]: StatePop[T] => StatePop[T] =
    (s: StatePop[T]) => {
      val numNew = state.testsManager.newTests.size
      state.testsManager.flushHelpers()  // adds newTests to all tests; resets newTests
      if (!silent) {
        val numKnown = state.testsManager.tests.values.count(_.isDefined)
        println(f"Tests: found: ${numNew}  total: ${state.testsManager.tests.size}  known outputs: $numKnown")
        // println(state.testsManager.tests.mkString("", "\n", "\n"))
      }
      s
    }
}



/**
  * A modified version of CDGPEvaluation which is specialized in accommodating steady state GP.
  * In steady state evaluated are, beside initial population, only single solutions selected in the
  * given iteration. In CDGP the number of test cases can increase, so updatePopulationEvalsAndTests
  * must be invoked every after creation of a new individual.
  *
  * @param state Object handling the state of the CDGP.
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
class CDGPEvaluationSteadyState[S, E](state: CDGPState,
                                      eval: S => E,
                                      updateEval: ((S, E)) => (S, E))
                                     (implicit opt: Options, coll: Collector, rng: TRandom)
  extends CDGPEvaluation[S, E](state, eval) {

  override def cdgpEvaluate = super.cdgpEvaluate andThen updatePopulationEvalsAndTests

  def updatePopulationEvalsAndTests(s: StatePop[(S, E)]): StatePop[(S, E)] = {
    val s2 =
      if (state.testsManager.newTests.nonEmpty)
        StatePop(s.map{ case (op, e) => updateEval(op, e) })
      else s
    updateTestsManager(s2)
  }
}
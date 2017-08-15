package sygusgp

import fuel.core.StatePop
import fuel.func.{Evaluation, SimpleSteadyStateEA}
import fuel.util.{Collector, Options, TRandom}
import swim.tree.{GPMoves, Op, SimpleGP}


/**
  * This file contains some ready to use genetic programming algorithms using
  * Counterexample-Driven Genetic Programming (CDGP) in various configurations.
  * You may use them as examples to construct your own variations.
  */



/**
  * This is standard generational GP in which evolved are program trees.
  * All solutions from the current generation are used to create offsprings (guided by the
  * selection process), and the new population is then populated by those offsprings.
  *
  * CDGP evaluation component is used to generate new counterexamples, and the test set
  * generally grows with the number of iterations.
  *
  * @param moves Definition of solution transformations, e.g. mutation, crossover.
  * @param cdgpEval CDGP evaluation component.
  * @param opt Options.
  * @param coll Collector for storing results and stats.
  * @param rng Pseudorandom numbers generator.
  * @param ordering Generates order on the fitness values.
  */
class CDGPGenerationalGP(moves: GPMoves,
                         cdgpEval: CDGPEvaluation[Op, Int])
                        (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[Int])
      extends SimpleGP(moves, (s:Op) => 0, Common.correctInt) {

  override def epilogue = super.epilogue andThen bsf// andThen reportStats(bsf) andThen epilogueGP(bsf)
  override def iter = super.iter andThen super.report// andThen Common.printPop
  override def evaluate: Evaluation[Op, Int] = cdgpEval
}

object CDGPGenerationalGP {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerationalGP = {
    val cdgpState = Common.getCDGPFactory(benchmark)
    val moves = GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible)
    val cdgpEval = new CDGPEvaluation(cdgpState, Common.evalInt(cdgpState.fitness))
    new CDGPGenerationalGP(moves, cdgpEval)
  }
}




/**
  * This is implementation of the steady state GP.
  * In a single iteration only one individual is selected, recombined and then it replaces
  * certain other individual (deselection process).
  *
  * CDGP evaluation component is used to generate new counterexamples, and the test set
  * generally grows with the number of iterations.
  *
  * @param moves Definition of solution transformations, e.g. mutation, crossover.
  * @param cdgpEval CDGP evaluation component.
  * @param opt Options.
  * @param coll Collector for storing results and stats.
  * @param rng Pseudorandom numbers generator.
  * @param ordering Generates order on the fitness values.
  */
class CDGPSteadyStateGP(moves: GPMoves,
                        cdgpEval: CDGPEvaluation[Op, Int])
                       (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[Int])
      extends SimpleSteadyStateEA[Op, Int](moves, (s:Op) => 0, Common.correctInt) {

  override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests
  override def epilogue = super.epilogue andThen bsf// andThen reportStats(bsf) andThen epilogueGP(bsf)
  override def evaluate: Evaluation[Op, Int] = cdgpEval
}

object CDGPSteadyStateGP {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyStateGP = {
    val cdgpState = Common.getCDGPFactory(benchmark)
    val moves = GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible)
    val cdgpEval = new CDGPEvaluationSteadyState(cdgpState, Common.evalInt(cdgpState.fitness),
      cdgpState.updateEvalInt)
    new CDGPSteadyStateGP(moves, cdgpEval)
  }
}




object Common {
  def correctInt: (Any, Int) => Boolean = (_: Any, e: Int) => e == -1
  def correctSeqInt: (Any, Seq[Int]) => Boolean = (_: Any, e: Seq[Int]) => e.nonEmpty && e.head == -1

  def evalInt(fitness: (Op) => (Boolean, Seq[Int]))
             (s: Op): Int = {
    val (isPerfect, r) = fitness(s)
    if (isPerfect) -1 // perfect program found; end of run
    else r.sum
  }

  def evalSeqInt(fitness: (Op) => (Boolean, Seq[Int]))
                (s: Op): Seq[Int] = {
    val (isPerfect, r) = fitness(s)
    if (isPerfect) r.map(_ => -1) // perfect program found; end of run
    else r
  }

  def printPop[E](s: StatePop[(Op, E)]): StatePop[(Op, E)] = {
    println(f"\nPopulation (size=${s.size}):")
    for (x <- s)
      println(x)
    println()
    s
  }

  def getCDGPFactory(benchmark: String)
                    (implicit opt: Options, coll: Collector, rng: TRandom): CDGPState = {
    new CDGPState(LoadSygusBenchmark(benchmark))
  }
}
package cdgp

import fuel.core.StatePop
import fuel.func._
import fuel.util.{Collector, Options, TRandom}
import swim.eval.LexicaseSelectionMain
import swim.tree._


/**
  * This file contains some ready to use genetic programming algorithms using
  * Counterexample-Driven Genetic Programming (CDGP) in various configurations.
  * You may use them as examples to construct your own variations.
  */



/**
  * This is a standard generational GP in which evolved are program trees.
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
class CDGPGenerational(moves: GPMoves,
                       cdgpEval: CDGPEvaluation[Op, FInt])
                      (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[FInt])
      extends SimpleGP(moves, cdgpEval.eval, Common.correctInt) {
  override def epilogue = super.epilogue andThen bsf andThen Common.epilogueEvalInt(cdgpEval.state, bsf)
  override def iter = super.iter// andThen super.report // uncomment report to change the result (FUEL issue #6)
  override def evaluate = cdgpEval
}

object CDGPGenerational {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerational = {
    apply(Common.getCDGPState(benchmark))
  }
  def apply(cdgpState: CDGPState)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerational = {
    implicit val ordering = FIntOrdering
    val moves = GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible)
    val cdgpEval = new CDGPEvaluation[Op, FInt](cdgpState, Common.evalInt(cdgpState.fitness))
    new CDGPGenerational(moves, cdgpEval)
  }
}




/**
  * This is an implementation of the steady state GP.
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
class CDGPSteadyState(moves: GPMoves,
                      cdgpEval: CDGPEvaluationSteadyState[Op, FInt])
                     (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[FInt])
      extends SimpleSteadyStateEA[Op, FInt](moves, cdgpEval.eval, Common.correctInt) {
  override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests
  override def epilogue = super.epilogue andThen bsf andThen Common.epilogueEvalInt(cdgpEval.state, bsf)
  override def evaluate = cdgpEval // used only for the initial population
  override def report = s => s
}

object CDGPSteadyState {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyState = {
    apply(Common.getCDGPState(benchmark))
  }
  def apply(cdgpState: CDGPState)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyState = {
    implicit val ordering = FIntOrdering
    val moves = GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible)
    val cdgpEval = new CDGPEvaluationSteadyState[Op, FInt](cdgpState, Common.evalInt(cdgpState.fitness),
      cdgpState.updateEvalInt)
    new CDGPSteadyState(moves, cdgpEval)
  }
}




/**
  * This is a standard generational GP in which evolved are program trees.
  * All solutions from the current generation are used to create offsprings (guided by the
  * selection process), and the new population is then populated by those offsprings.
  *
  * This version of CDGP utilizes lexicase selection, which proves very effective in practice.
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
class CDGPGenerationalLexicase(moves: GPMoves,
                               cdgpEval: CDGPEvaluation[Op, FSeqInt])
                              (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[Int])
      extends LexicaseGPMain[Int, FSeqInt](moves, cdgpEval.eval, Common.correctSeqInt, FSeqIntOrdering) {
  override def epilogue = super.epilogue andThen bsf andThen Common.epilogueEvalSeqInt(cdgpEval.state, bsf)
  override def iter = super.iter andThen super.report
  override def evaluate = cdgpEval
}

object CDGPGenerationalLexicase {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerationalLexicase = {
    apply(Common.getCDGPState(benchmark))
  }
  def apply(cdgpState: CDGPState)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerationalLexicase = {
    implicit val ordering = FSeqIntOrdering
    val moves = GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible)
    val cdgpEval = new CDGPEvaluation[Op, FSeqInt](cdgpState, Common.evalSeqInt(cdgpState.fitness))
    new CDGPGenerationalLexicase(moves, cdgpEval)
  }
}




/**
  * This is an implementation of the steady state GP.
  * In a single iteration only one individual is selected, recombined and then it replaces
  * certain other individual (deselection process).
  *
  * This version of CDGP utilizes lexicase selection, which proves very effective in practice.
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
class CDGPSteadyStateLexicase(moves: GPMoves,
                              cdgpEval: CDGPEvaluationSteadyState[Op, FSeqInt])
                             (implicit opt: Options, coll: Collector, rng: TRandom,
                              ordering: Ordering[FSeqInt])
      extends SteadyStateEA[Op, FSeqInt](moves, cdgpEval.eval,
                                          Common.correctSeqInt,
                                          CDGPSteadyStateLexicase.getSelection(),
                                          CDGPSteadyStateLexicase.getDeselection()) {
  override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests
  override def epilogue = super.epilogue andThen bsf andThen Common.epilogueEvalSeqInt(cdgpEval.state, bsf)
  override def evaluate = cdgpEval // used only for the initial population
}

object CDGPSteadyStateLexicase {
  def getSelection()(implicit rng: TRandom): Selection[Op, FSeqInt] =
    new LexicaseSelectionMain[Op, Int, FSeqInt](Ordering[Int])

  def getDeselection()(implicit opt: Options, rng: TRandom): Selection[Op, FSeqInt] =
    if (opt('lexicaseDeselection, false)) new LexicaseSelectionMain[Op, Int, FSeqInt](Ordering[Int].reverse)
    else new TournamentSelection[Op, FSeqInt](FSeqIntOrdering.reverse)

  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyStateLexicase = {
    apply(Common.getCDGPState(benchmark))
  }
  def apply(cdgpState: CDGPState)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyStateLexicase = {
    implicit val ordering = FSeqIntOrdering
    val moves = GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible)
    val cdgpEval = new CDGPEvaluationSteadyState[Op, FSeqInt](cdgpState, Common.evalSeqInt(cdgpState.fitness),
      cdgpState.updateEvalSeqInt)
    new CDGPSteadyStateLexicase(moves, cdgpEval)
  }
}




object Common {
  def correctInt: (Any, FInt) => Boolean =
    (_: Any, e: FInt) => e.correct
  def correctSeqInt: (Any, FSeqInt) => Boolean =
    (_: Any, e: FSeqInt) => e.correct

  def evalInt(fitness: (Op) => (Boolean, Seq[Int]))(s: Op): FInt = {
    val (isPerfect, r) = fitness(s)
    FInt(isPerfect, r.sum)
  }
  def evalSeqInt(fitness: (Op) => (Boolean, Seq[Int]))(s: Op): FSeqInt = {
    val (isPerfect, r) = fitness(s)
    FSeqInt(isPerfect, r)
  }

  def printPop[S, E](s: StatePop[(S, E)]): StatePop[(S, E)] = {
    println(f"\nPopulation (size=${s.size}):")
    for (x <- s)
      println(x)
    println()
    s
  }

  def getCDGPState(benchmark: String)
                  (implicit opt: Options, coll: Collector, rng: TRandom): CDGPState = {
    new CDGPState(LoadSygusBenchmark(benchmark))
  }

  def epilogueEvalInt(cdgpState: CDGPState, bsf: BestSoFar[Op, FInt])
                     (s: StatePop[(Op, FInt)])
                     (implicit opt: Options, coll: Collector): StatePop[(Op, FInt)] = {
    val (_, e) = bsf.bestSoFar.get
    val totalTests = cdgpState.testsManager.getNumberOfTests
    val passedTests = if (e.correct) totalTests else totalTests - e.value
    val ratio = if (totalTests == 0) 1.0 else passedTests / totalTests
    val roundedRatio = BigDecimal(ratio).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.setResult("best.passedTests", passedTests)
    coll.setResult("best.numTests", totalTests)
    coll.setResult("best.passedTestsRatio", roundedRatio)
    coll.setResult("best.isOptimal", e.correct)
    coll.setResult("best.isApproximate", !e.correct)
    reportStats(cdgpState, bsf)(s)
  }

  def epilogueEvalSeqInt(cdgpState: CDGPState, bsf: BestSoFar[Op, FSeqInt])
                        (s: StatePop[(Op, FSeqInt)])
                        (implicit opt: Options, coll: Collector): StatePop[(Op, FSeqInt)] = {
    val (_, e) = bsf.bestSoFar.get
    val passedTests = if (e.correct) e.size else e.count(_ == 0)
    val ratio = if (e.isEmpty) 1.0 else passedTests / e.size.toDouble
    val roundedRatio = BigDecimal(ratio).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.setResult("best.passedTests", passedTests)
    coll.setResult("best.numTests", e.size)
    coll.setResult("best.passedTestsRatio", roundedRatio)
    coll.setResult("best.isOptimal", e.correct)
    coll.setResult("best.isApproximate", !e.correct)
    reportStats(cdgpState, bsf)(s)
  }

  def reportStats[E](cdgpState: CDGPState, bsf: BestSoFar[Op, E])
                    (s: StatePop[(Op, E)])
                    (implicit opt: Options, coll: Collector): StatePop[(Op, E)] = {
    if (bsf.bestSoFar.isDefined) {
      val (bestOfRun, _) = bsf.bestSoFar.get
      coll.set("result.best", bestOfRun)
      coll.set("result.best.smtlib", SMTLIBFormatter.opToString(bestOfRun))
      coll.set("result.best.size", bsf.bestSoFar.get._1.size)
      coll.set("result.best.height", bsf.bestSoFar.get._1.height)
    }
    coll.set("cdgp.totalTests", cdgpState.testsManager.tests.size)
    coll.set("cdgp.testsHistory", cdgpState.testsManager.history.mkString(","))
    coll.set("cdgp.totalTestsKnownOutputs", cdgpState.testsManager.getNumberOfKnownOutputs)
    coll.set("cdgp.totalTestsUnknownOutputs", cdgpState.testsManager.getNumberOfUnknownOutputs)
    coll.set("cdgp.totalSolverCalls", cdgpState.solver.getNumCalls)
    coll.set("cdgp.totalSolverRestarts", cdgpState.solver.getNumRestarts)
    coll.saveSnapshot("cdgp")
    s
  }
}



case class FSeqInt(correct: Boolean, value: Seq[Int]) extends Seq[Int] {
  override def length = value.length
  override def apply(idx: Int) = value(idx)
  override def iterator = value.iterator
  override def toString: String = f"Fit($correct, $value)"
}
case class FInt(correct: Boolean, value: Int) {
  override def toString: String = f"Fit($correct, $value)"
}

object FSeqIntOrdering extends Ordering[FSeqInt] {
  def compare(a: FSeqInt, b: FSeqInt): Int = {
    if (a.correct && !b.correct) -1
    else if (!a.correct && b.correct) 1
    else LongerOrMaxPassedOrdering.compare(a.value, b.value)
  }
}
object FIntOrdering extends Ordering[FInt] {
  def compare(a: FInt, b: FInt): Int = {
    if (a.correct && !b.correct) -1
    else if (!a.correct && b.correct) 1
    else a.value compare b.value
  }
}
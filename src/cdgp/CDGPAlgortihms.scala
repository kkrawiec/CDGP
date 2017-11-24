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



trait CDGPAlgorithm[S, E] {
  def cdgpState: CDGPState
  def bsf: BestSoFar[S, E]

  /**
    * Current state of the population. Should be updated after every iteration.
    */
  var pop: Option[StatePop[(S, E)]] = None
}



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
      extends SimpleGP(moves, cdgpEval.eval, Common.correctInt) with CDGPAlgorithm[Op, FInt] {
  override def cdgpState: CDGPState = cdgpEval.state
  override def initialize  = super.initialize
  override def epilogue = super.epilogue andThen bsf andThen Common.epilogueEvalInt(cdgpEval.state, bsf)
  override def iter = super.iter andThen Common.saveCurrentPop(this)// andThen super.report // uncomment report to change the result (FUEL issue #6)
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
    val moves = GPMoves(cdgpState.grammar, Common.isFeasible(cdgpState.synthTask.fname, opt))
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
      extends SteadyStateEA[Op, FInt](moves, cdgpEval.eval,
                                      Common.correctInt,
                                      CDGPSteadyState.getSelection(),
                                      CDGPSteadyState.getDeselection()) with CDGPAlgorithm[Op, FInt] {
  override def cdgpState: CDGPState = cdgpEval.state
  override def initialize  = super.initialize
  override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests andThen Common.saveCurrentPop(this)
  override def epilogue = super.epilogue andThen bsf andThen Common.epilogueEvalInt(cdgpEval.state, bsf)
  override def report = s => s
  override def evaluate = // used only for the initial population
    (s: StatePop[Op]) => {
      cdgpEval.state.testsManager.flushHelpers()
      if (cdgpEval.state.testsManager.tests.isEmpty)
        Common.evalPopToDefaultFInt(false, 0)(s)
      else
        Common.evalPopNoVerificationFInt(cdgpEval.state)(s)
    }
}

object CDGPSteadyState {
  def getSelection()(implicit opt: Options, rng: TRandom): Selection[Op, FInt] =
    new TournamentSelection(FIntOrdering, opt('tournamentSize, 7, (_: Int) >= 2))

  def getDeselection()(implicit opt: Options, rng: TRandom): Selection[Op, FInt] = {
    val k = opt('tournamentSize, 7, (_: Int) >= 2)
    new TournamentSelection(FIntOrdering.reverse, opt('tournamentDeselectSize, k, (_: Int) >= 2))
  }

  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyState = {
    apply(Common.getCDGPState(benchmark))
  }
  def apply(cdgpState: CDGPState)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyState = {
    implicit val ordering = FIntOrdering
    val moves = GPMoves(cdgpState.grammar, Common.isFeasible(cdgpState.synthTask.fname, opt))
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
      extends LexicaseGPMain[Int, FSeqInt](moves, cdgpEval.eval, Common.correctSeqInt, FSeqIntOrdering)
        with CDGPAlgorithm[Op, FSeqInt] {
  override def cdgpState: CDGPState = cdgpEval.state
  override def initialize  = super.initialize
  override def epilogue = super.epilogue andThen bsf andThen Common.epilogueEvalSeqInt(cdgpEval.state, bsf)
  override def iter = super.iter andThen super.report andThen Common.saveCurrentPop(this)
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
    val moves = GPMoves(cdgpState.grammar, Common.isFeasible(cdgpState.synthTask.fname, opt))
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
                                         CDGPSteadyStateLexicase.getDeselection()) with CDGPAlgorithm[Op, FSeqInt] {
  override def cdgpState: CDGPState = cdgpEval.state
  override def initialize  = super.initialize
  override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests andThen Common.saveCurrentPop(this)
  override def epilogue = super.epilogue andThen bsf andThen Common.epilogueEvalSeqInt(cdgpEval.state, bsf)
  override def evaluate = // used only for the initial population
    (s: StatePop[Op]) => {
      cdgpEval.state.testsManager.flushHelpers()
      if (cdgpEval.state.testsManager.tests.isEmpty)
        Common.evalPopToDefaultFSeqInt(false, List())(s)
      else
        Common.evalPopNoVerificationFSeqInt(cdgpEval.state)(s)
    }
}

object CDGPSteadyStateLexicase {
  def getSelection()(implicit rng: TRandom): Selection[Op, FSeqInt] =
    new LexicaseSelectionMain[Op, Int, FSeqInt](Ordering[Int])

  def getDeselection()(implicit opt: Options, rng: TRandom): Selection[Op, FSeqInt] =
    if (opt('lexicaseDeselection, false)) new LexicaseSelectionMain[Op, Int, FSeqInt](Ordering[Int].reverse)
    else {
      val k = opt('tournamentSize, 7, (_: Int) >= 2)
      new TournamentSelection[Op, FSeqInt](FSeqIntOrdering.reverse, opt('tournamentDeselectSize, k, (_: Int) >= 2))
    }

  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyStateLexicase = {
    apply(Common.getCDGPState(benchmark))
  }
  def apply(cdgpState: CDGPState)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyStateLexicase = {
    implicit val ordering = FSeqIntOrdering
    val moves = GPMoves(cdgpState.grammar, Common.isFeasible(cdgpState.synthTask.fname, opt))
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

  def containsDiv0(op: Op): Boolean = {
    if (op.args.isEmpty)
      false
    else if ((op.op == 'mod || op.op == 'div) && op.args(1).op == 0)
      true
    else
      op.args.exists(containsDiv0(_))
  }
  def isFeasible(rootFunName: String, opt: Options)(op: Op): Boolean = {
    if (op.count(rootFunName) <= opt.paramInt("maxRecursiveCalls", 1) && !containsDiv0(op))
      SimpleGP.defaultFeasible(opt)(op)
    else false
  }

  def evalInt(fitness: (Op) => (Boolean, Seq[Int]))(s: Op): FInt = {
    val (isPerfect, r) = fitness(s)
    FInt(isPerfect, r.sum, s.size)
  }
  def evalSeqInt(fitness: (Op) => (Boolean, Seq[Int]))(s: Op): FSeqInt = {
    val (isPerfect, r) = fitness(s)
    FSeqInt(isPerfect, r, s.size)
  }

  def evalPopNoVerificationFSeqInt(state: CDGPState)(s: StatePop[Op]): StatePop[(Op, FSeqInt)] = {
    StatePop(s.map { op =>
      val f = state.fitnessNoVerification(op)
      (op, FSeqInt(f._1, f._2, op.size))
    })
  }
  def evalPopToDefaultFSeqInt(dec: Boolean, fit: Seq[Int])(s: StatePop[Op]): StatePop[(Op, FSeqInt)] = {
    StatePop(s.map{ op => (op, FSeqInt(dec, fit, op.size))})
  }

  def evalPopNoVerificationFInt(state: CDGPState)(s: StatePop[Op]): StatePop[(Op, FInt)] = {
    StatePop(s.map{ op =>
      val f = state.fitnessNoVerification(op)
      (op, FInt(f._1, f._2.sum, op.size))
    })
  }
  def evalPopToDefaultFInt(dec: Boolean, fit: Int)(s: StatePop[Op]): StatePop[(Op, FInt)] = {
    StatePop(s.map{ op => (op, FInt(dec, fit, op.size))})
  }

  def saveCurrentPop[S, E](alg: CDGPAlgorithm[S, E])(s: StatePop[(S, E)]): StatePop[(S, E)] = {
    alg.pop = Some(s)
    s
  }

  def printPop[S, E](s: StatePop[(S, E)]): StatePop[(S, E)] = {
    println(s"\nPopulation (size=${s.size}):")
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
    val ratio = if (totalTests == 0) 1.0 else passedTests.toDouble / totalTests
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
    val ratio = if (e.isEmpty) 1.0 else passedTests.toDouble / e.size
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
      val solutionOriginal = SMTLIBFormatter.opToString(bestOfRun)
      val solutionSimplified = cdgpState.simplifySolution(solutionOriginal)
      val solution = solutionSimplified.getOrElse(solutionOriginal)
      val solutionOp = SMTLIBFormatter.smtlibToOp(solution)

      coll.set("result.bestOrig", bestOfRun)
      coll.set("result.bestOrig.smtlib", solutionOriginal)
      coll.set("result.bestOrig.size", bestOfRun.size)
      coll.set("result.bestOrig.height", bestOfRun.height)

      coll.set("result.best", solutionOp)
      coll.set("result.best.smtlib", solution)
      coll.set("result.best.size", solutionOp.size)
      coll.set("result.best.height", solutionOp.height)
    }
    coll.set("cdgp.totalTests", cdgpState.testsManager.tests.size)
    coll.set("cdgp.testsHistory", cdgpState.testsManager.history.toList.sorted.mkString(", "))
    coll.set("cdgp.totalTestsKnownOutputs", cdgpState.testsManager.getNumberOfKnownOutputs)
    coll.set("cdgp.totalTestsUnknownOutputs", cdgpState.testsManager.getNumberOfUnknownOutputs)
    coll.set("cdgp.numRejectedCounterex", cdgpState.numRejectedCounterex)
    coll.set("cdgp.solverTotalCalls", cdgpState.solver.getNumCalls)
    coll.set("cdgp.solverTotalRestarts", cdgpState.solver.getNumRestarts)
    coll.set("cdgp.solverTimeMinSec", cdgpState.solver.getMinSolveTime)
    coll.set("cdgp.solverTimeMaxSec", cdgpState.solver.getMaxSolveTime)
    coll.set("cdgp.solverTimeAvgSec", cdgpState.solver.getAvgSolveTime)
    coll.set("cdgp.solverTimeSumSec", cdgpState.solver.getSumSolveTime)
    coll.set("cdgp.solverAllTimesCountMap", cdgpState.solver.getSolveTimesAsCountMap.toList.sortBy(_._1).mkString(", "))
    s
  }
}



case class FSeqInt(correct: Boolean, value: Seq[Int], progSize: Int) extends Seq[Int] {
  override def length = value.length
  override def apply(idx: Int) = value(idx)
  override def iterator = value.iterator
  override def toString: String = s"Fit($correct, $value, progSize=$progSize)"
}
case class FInt(correct: Boolean, value: Int, progSize: Int) {
  override def toString: String = s"Fit($correct, $value, progSize=$progSize)"
}

object FSeqIntOrdering extends Ordering[FSeqInt] {
  override def compare(a: FSeqInt, b: FSeqInt): Int = {
    val c = if (a.correct && !b.correct) -1
            else if (!a.correct && b.correct) 1
            else LongerOrMaxPassedOrdering.compare(a.value, b.value)
    // lexicographic parsimony pressure
    if (c == 0) a.progSize compare b.progSize
    else c
  }
}
object FIntOrdering extends Ordering[FInt] {
  override def compare(a: FInt, b: FInt): Int = {
    val c = if (a.correct && !b.correct) -1
            else if (!a.correct && b.correct) 1
            else a.value compare b.value
    // lexicographic parsimony pressure
    if (c == 0) a.progSize compare b.progSize
    else c
  }
}
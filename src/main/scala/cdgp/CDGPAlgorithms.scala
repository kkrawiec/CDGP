package cdgp

import fuel.core.StatePop
import fuel.func.{EACore, _}
import fuel.util.{CallEvery, Collector, Options, TRandom}
import swim.eval.{EpsLexicaseSelection, LexicaseSelection01}
import swim.tree._


/**
  * This file contains some ready to use genetic programming algorithms using
  * Counterexample-Driven Genetic Programming (CDGP) in various configurations.
  * You may use them as examples to construct your own variations.
  */




/**
  * Trait for common aspects of CDGP algorithms defined in this file.
  * @tparam S Class representing solutions
  * @tparam E Class representing evaluations
  */
trait CDGPAlgorithm[S <: Op, E <: Fitness] {
  def cdgpState: State
  def bsf: BestSoFar[S, E]

  /**
    * Current state of the population. Should be updated after every iteration.
    */
  var pop: Option[StatePop[(S, E)]] = None

  /**
    * Number of generations until the algorithm ended.
    */
  var numGenerations = 0

  /**
    * Saves the provided state of the population and returns it, so that it can
    * be used further in the pipe.
    */
  def updateAfterIteration(s: StatePop[(S, E)]): StatePop[(S, E)] = {
    pop = Some(s)
    numGenerations += 1
    s
  }

  def reportStats(s: StatePop[(S, E)])
                 (implicit opt: Options, coll: Collector): StatePop[(S, E)] = {
    if (bsf.bestSoFar.isDefined) {
      val (bestOfRun, e) = bsf.bestSoFar.get
      e.saveInColl(coll)
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

      coll.set("result.totalGenerations", numGenerations)
    }
    cdgpState.reportData()
    s
  }
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
class CDGPGenerational[E <: Fitness]
                      (moves: GPMoves,
                       cdgpEval: CDGPEvaluation[Op, E])
                      (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
      extends SimpleGP(moves, cdgpEval.eval, Common.correct(cdgpEval.eval)) with CDGPAlgorithm[Op, E] {
  override def cdgpState = cdgpEval.state
  override def initialize  = super.initialize
  override def epilogue = super.epilogue andThen bsf andThen reportStats
  override def iter = super.iter andThen updateAfterIteration // andThen super.report // uncomment report to change the result (FUEL issue #6)
  override def evaluate = cdgpEval
}

object CDGPGenerational {
  def apply[E <: Fitness](eval: EvalFunction[Op, E])
                         (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerational[E] = {
    implicit val ordering = eval.ordering
    val grammar = eval.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(eval.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluation[Op, E](eval)
    new CDGPGenerational[E](moves, cdgpEval)
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
      extends SteadyStateEA[Op, FInt](moves, cdgpEval.eval, Common.correct(cdgpEval.eval),
                                      CDGPSteadyState.getSelection(),
                                      CDGPSteadyState.getDeselection()) with CDGPAlgorithm[Op, FInt] {
  override def cdgpState = cdgpEval.state
  override def initialize  = super.initialize
  override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests andThen updateAfterIteration
  override def epilogue = super.epilogue andThen bsf andThen reportStats
  override def report = s => s
  override def evaluate = // used only for the initial population
    (s: StatePop[Op]) => {
      cdgpEval.state.testsManager.flushHelpers()
      if (cdgpEval.state.testsManager.tests.isEmpty)
        Common.evalPopToDefault(cdgpEval.eval)(s)
      else
        StatePop(s.map{ op => (op, cdgpEval.eval(op, true)) })
    }
}

object CDGPSteadyState {
  def getSelection()(implicit opt: Options, rng: TRandom): Selection[Op, FInt] =
    new TournamentSelection(FIntOrdering, opt('tournamentSize, 7, (_: Int) >= 2))

  def getDeselection()(implicit opt: Options, rng: TRandom): Selection[Op, FInt] = {
    val k = opt('tournamentSize, 7, (_: Int) >= 2)
    new TournamentSelection(FIntOrdering.reverse, opt('tournamentDeselectSize, k, (_: Int) >= 2))
  }

  def apply(eval: EvalFunction[Op, FInt])
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyState = {
    implicit val ordering = eval.ordering
    val grammar = eval.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(eval.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluationSteadyState[Op, FInt](eval, eval.updateEval)
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
  extends LexicaseGPMain[Int, FSeqInt](moves, cdgpEval.eval, Common.correct(cdgpEval.eval), FSeqIntOrdering)
     with CDGPAlgorithm[Op, FSeqInt] {
  override val selection = new LexicaseSelection01[Op, FSeqInt]
  override def cdgpState = cdgpEval.state
  override def initialize  = super.initialize
  override def epilogue = super.epilogue andThen bsf andThen reportStats
  override def iter = super.iter andThen super.report andThen updateAfterIteration
  override def evaluate = cdgpEval
}

object CDGPGenerationalLexicase {
  def apply(eval: EvalFunction[Op, FSeqInt])
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerationalLexicase = {
    implicit val ordering = eval.ordering
    val grammar = eval.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(eval.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluation[Op, FSeqInt](eval)
    new CDGPGenerationalLexicase(moves, cdgpEval)
  }
}





class CDGPGenerationalLexicaseR(moves: GPMoves,
                                cdgpEval: CDGPEvaluation[Op, FSeqDouble])
                               (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[FSeqDouble])
  extends EACore[Op, FSeqDouble](moves, SequentialEval(cdgpEval.eval), Common.correct(cdgpEval.eval))
     with CDGPAlgorithm[Op, FSeqDouble] {
  val bsf = BestSoFar[Op, FSeqDouble](ordering, it)
  override def cdgpState = cdgpEval.state
  override def initialize = super.initialize// andThen Common.printPop
  override def epilogue = super.epilogue andThen bsf andThen reportStats
  override def iter = (s: StatePop[(Op, FSeqDouble)]) =>
    (createBreeder(s) andThen evaluate andThen super.report andThen updateAfterIteration)(s)
  override def evaluate = cdgpEval

  def createBreeder(s: StatePop[(Op, FSeqDouble)]): (StatePop[(Op, FSeqDouble)] => StatePop[Op]) = {
    val epsForTests = EpsLexicaseSelection.medianAbsDev(s)
    val sel = new EpsLexicaseSelection[Op, FSeqDouble](epsForTests)
    SimpleBreeder(sel, moves: _*)
  }
}

object CDGPGenerationalLexicaseR {
  def apply(eval: EvalFunction[Op, FSeqDouble])
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerationalLexicaseR = {
    implicit val ordering = eval.ordering
    val grammar = eval.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(eval.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluation[Op, FSeqDouble](eval)
    new CDGPGenerationalLexicaseR(moves, cdgpEval)
  }
}





class CDGPGenerationalCore[E <: Fitness]
                          (selection: Selection[Op, E],
                           moves: GPMoves,
                           cdgpEval: CDGPEvaluation[Op, E])
                          (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
  extends EACore[Op, E](moves, SequentialEval(cdgpEval.eval), Common.correct(cdgpEval.eval))
     with CDGPAlgorithm[Op, E] {

  override def cdgpState = cdgpEval.state
  val bsf = BestSoFar[Op, E](ordering, it)
  override def iter = SimpleBreeder(selection, moves: _*) andThen evaluate

  override def initialize  = super.initialize
  override def epilogue = super.epilogue andThen bsf andThen reportStats
  //override def iter = super.iter andThen super.report andThen saveCurrentPop
  override def evaluate = cdgpEval
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
class CDGPSteadyStateLexicase[E <: Fitness](moves: GPMoves,
                              cdgpEval: CDGPEvaluationSteadyState[Op, E],
                              selection: Selection[Op, E],
                              deselection: Selection[Op, E])
                             (implicit opt: Options, coll: Collector, rng: TRandom,
                              ordering: Ordering[E])
      extends SteadyStateEA[Op, E](moves, cdgpEval.eval, Common.correct(cdgpEval.eval),
        selection, deselection) with CDGPAlgorithm[Op, E] {
  override def cdgpState = cdgpEval.state
  override def initialize  = super.initialize
  override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests andThen updateAfterIteration
  override def epilogue = super.epilogue andThen bsf andThen reportStats
  override def evaluate = // used only for the initial population
    (s: StatePop[Op]) => {
      cdgpEval.state.testsManager.flushHelpers()
      if (cdgpEval.state.testsManager.tests.isEmpty)
        Common.evalPopToDefault(cdgpEval.eval)(s)
      else
        StatePop(s.map{ op => (op, cdgpEval.eval(op, true)) })
    }
}

object CDGPSteadyStateLexicase {
  def getSelection()(implicit rng: TRandom): Selection[Op, FSeqInt] =
    new LexicaseSelection01[Op, FSeqInt]

  def getDeselection(eval: EvalFunction[Op, FSeqInt])(implicit opt: Options, rng: TRandom): Selection[Op, FSeqInt] =
    if (opt('lexicaseDeselection, false)) new LexicaseSelection01[Op, FSeqInt]
    else {
      val k = opt('tournamentSize, 7, (_: Int) >= 2)
      new TournamentSelection[Op, FSeqInt](eval.ordering.reverse, k)
    }

  def apply(eval: EvalFunction[Op, FSeqInt])
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyStateLexicase[FSeqInt] = {
    implicit val ordering = eval.ordering
    val grammar = eval.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(eval.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluationSteadyState[Op, FSeqInt](eval, eval.updateEval)
    val sel = CDGPSteadyStateLexicase.getSelection()
    val desel = CDGPSteadyStateLexicase.getDeselection(eval)
    new CDGPSteadyStateLexicase(moves, cdgpEval, sel, desel)
  }
}




class CDGPSteadyStateLexicaseR[E <: FSeqDouble](moves: GPMoves,
                              cdgpEval: CDGPEvaluationSteadyState[Op, E],
                              deselection: Selection[Op, E])
                             (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
  extends EACore[Op, E](moves, SequentialEval(cdgpEval.eval), Common.correct(cdgpEval.eval))
     with CDGPAlgorithm[Op, E] {

  val bsf = BestSoFar[Op, E](ordering, it)
  val n = opt('reportFreq, opt('populationSize, 1000))

  override def cdgpState = cdgpEval.state
  override def initialize  = super.initialize
  override def iter = (s: StatePop[(Op, E)]) => (createBreeder(s) andThen
    CallEvery(n, report) andThen
    cdgpEval.updatePopulationEvalsAndTests andThen
    updateAfterIteration)(s)
  override def epilogue = super.epilogue andThen bsf andThen reportStats
  override def report = bsf
  override def evaluate = // used only for the initial population
    (s: StatePop[Op]) => {
      cdgpEval.state.testsManager.flushHelpers()
      if (cdgpEval.state.testsManager.tests.isEmpty)
        Common.evalPopToDefault(cdgpEval.eval)(s)
      else
        StatePop(s.map{ op => (op, cdgpEval.eval(op, true)) })
    }

  def createBreeder(s: StatePop[(Op, E)]): (StatePop[(Op, E)] => StatePop[(Op, E)]) = {
    val epsForTests = EpsLexicaseSelection.medianAbsDev(s)
    val sel = new EpsLexicaseSelection[Op, E](epsForTests)
    new SimpleSteadyStateBreeder[Op, E](sel, RandomMultiOperator(moves: _*),
      deselection, cdgpEval.eval)
  }
}

object CDGPSteadyStateLexicaseR {
  def getDeselection[E <: FSeqDouble](eval: EvalFunction[Op, E])(implicit opt: Options, rng: TRandom): Selection[Op, E] = {
    val k = opt('tournamentSize, 7, (_: Int) >= 2)
    new TournamentSelection[Op, E](eval.ordering.reverse, k)
  }

  def apply[E <: FSeqDouble](eval: EvalFunction[Op, E])
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyStateLexicaseR[E] = {
    implicit val ordering = eval.ordering
    val grammar = eval.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(eval.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluationSteadyState[Op, E](eval, eval.updateEval)
    val desel = CDGPSteadyStateLexicaseR.getDeselection(eval)
    new CDGPSteadyStateLexicaseR[E](moves, cdgpEval, desel)
  }
}






object Common {
  def correct[S, E](eval: EvalFunction[S, E]): (S, E) => Boolean =
    (_: S, e: E) => eval.correct(e)

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

  def evalPopToDefault[S, E](eval: EvalFunction[S, E])(s: StatePop[S]): StatePop[(S, E)] = {
    StatePop(s.map{ op => (op, eval.defaultValue(op))})
  }

  def printPop[S, E](s: StatePop[(S, E)]): StatePop[(S, E)] = {
    println(s"\nPopulation (size=${s.size}):")
    for (x <- s)
      println(x)
    println()
    s
  }
}

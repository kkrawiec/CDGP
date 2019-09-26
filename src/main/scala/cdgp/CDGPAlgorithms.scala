package cdgp

import fuel.core.StatePop
import fuel.func._
import fuel.util.{CallCounter, CallEvery, Collector, Options, TRandom}
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
                       cdgpEval: CDGPEvaluation[Op, E],
                       correct: (Op, E) => Boolean)
                      (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
      extends SimpleGP(moves, cdgpEval.eval, correct) with CDGPAlgorithm[Op, E] {
  override def cdgpState = cdgpEval.state
  override def initialize  = super.initialize
  override def epilogue = super.epilogue andThen bsf andThen reportStats
  override def iter = super.iter andThen updateAfterIteration // andThen super.report // uncomment report to change the result (FUEL issue #6)
  override def evaluate = cdgpEval
  override def algorithm =
    (s: StatePop[(Op, E)]) =>  Common.restartLoop(initialize, super.algorithm andThen bsf, correct, it, bsf, opt)(s)
//  override def report = s => s
}

object CDGPGenerational {
  def apply[E <: Fitness](eval: EvalFunction[Op, E])
                         (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerational[E] = {
    implicit val ordering = eval.ordering
    val grammar = eval.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(eval.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluation[Op, E](eval)
    val correct = Common.correct(cdgpEval.eval)
    new CDGPGenerational[E](moves, cdgpEval, correct)
  }
}



class CDGPGenerationalR[E <: Fitness]
                       (moves: GPMoves,
                        cdgpEval: CDGPEvaluation[Op, E],
                        correct: (Op, E) => Boolean)
                       (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
  extends CDGPGenerational(moves, cdgpEval, correct) {
  override def initialize = {
    Common.addNoiseToTests(cdgpEval.state)(opt, rng); super.initialize
  }
}

object CDGPGenerationalR {
  def apply[E <: Fitness](eval: EvalFunction[Op, E])
                         (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerational[E] = {
    implicit val ordering = eval.ordering
    val grammar = eval.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(eval.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluation[Op, E](eval)
    val correct = Common.correct(cdgpEval.eval)
    new CDGPGenerationalR[E](moves, cdgpEval, correct)
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
                      cdgpEval: CDGPEvaluationSteadyState[Op, FInt],
                      correct: (Op, FInt) => Boolean)
                     (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[FInt])
      extends SteadyStateEA[Op, FInt](moves, cdgpEval.eval, correct,
                                      CDGPSteadyState.getSelection(),
                                      CDGPSteadyState.getDeselection()) with CDGPAlgorithm[Op, FInt] {
  override def cdgpState = cdgpEval.state
  override def initialize  = super.initialize
  override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests andThen updateAfterIteration andThen bsf
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
  override def algorithm =
    (s: StatePop[(Op, FInt)]) =>  Common.restartLoop(initialize, super.algorithm andThen bsf, correct, it, bsf, opt)(s)
}

object CDGPSteadyState {
  def getSelection()(implicit opt: Options, rng: TRandom): Selection[Op, FInt] =
    new TournamentSelection(FIntOrdering, opt('tournamentSize, 7))

  def getDeselection()(implicit opt: Options, rng: TRandom): Selection[Op, FInt] = {
    val k = opt('tournamentSize, 7)
    new TournamentSelection(FIntOrdering.reverse, opt('tournamentDeselectSize, k))
  }

  def apply(eval: EvalFunction[Op, FInt])
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyState = {
    implicit val ordering = eval.ordering
    val grammar = eval.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(eval.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluationSteadyState[Op, FInt](eval, eval.updateEval)
    val correct = Common.correct(cdgpEval.eval)
    new CDGPSteadyState(moves, cdgpEval, correct)
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
                               cdgpEval: CDGPEvaluation[Op, FSeqInt],
                               correct: (Op, FSeqInt) => Boolean)
                              (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[Int])
  extends LexicaseGPMain[Int, FSeqInt](moves, cdgpEval.eval, correct, FSeqIntOrdering)
     with CDGPAlgorithm[Op, FSeqInt] {
  override val selection = new LexicaseSelection01[Op, FSeqInt]
  override def cdgpState = cdgpEval.state
  override def initialize  = super.initialize
  override def epilogue = super.epilogue andThen bsf andThen reportStats
  override def iter = super.iter andThen updateAfterIteration
  override def evaluate = cdgpEval
  override def report = s => s
  override def algorithm =
    (s: StatePop[(Op, FSeqInt)]) =>  Common.restartLoop(initialize, super.algorithm andThen bsf, correct, it, bsf, opt)(s)
}

object CDGPGenerationalLexicase {
  def apply(eval: EvalFunction[Op, FSeqInt])
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerationalLexicase = {
    implicit val ordering = eval.ordering
    val grammar = eval.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(eval.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluation[Op, FSeqInt](eval)
    val correct = Common.correct(cdgpEval.eval)
    new CDGPGenerationalLexicase(moves, cdgpEval, correct)
  }
}





class CDGPGenerationalLexicaseR(moves: GPMoves,
                                cdgpEval: CDGPEvaluation[Op, FSeqDouble],
                                correct: (Op, FSeqDouble) => Boolean)
                               (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[FSeqDouble])
  extends EACore[Op, FSeqDouble](moves, SequentialEval(cdgpEval.eval), correct)
     with CDGPAlgorithm[Op, FSeqDouble] {
  val bsf = BestSoFar[Op, FSeqDouble](ordering, it)
  override def cdgpState = cdgpEval.state
  override def initialize = {
    Common.addNoiseToTests(cdgpEval.state)(opt, rng); super.initialize
  }
  override def epilogue = super.epilogue andThen bsf andThen reportStats
  override def iter = (s: StatePop[(Op, FSeqDouble)]) =>
    (createBreeder(s) andThen evaluate andThen updateAfterIteration)(s)
  override def evaluate = cdgpEval
  override def report = s => s
  override def algorithm =
    (s: StatePop[(Op, FSeqDouble)]) =>  Common.restartLoop(initialize, super.algorithm andThen bsf, correct, it, bsf, opt)(s)

  def createBreeder(s: StatePop[(Op, FSeqDouble)]): StatePop[(Op, FSeqDouble)] => StatePop[Op] = {
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
    val correct = Common.correct(cdgpEval.eval)
    new CDGPGenerationalLexicaseR(moves, cdgpEval, correct)
  }
}




// currently not used anywhere
class CDGPGenerationalCore[E <: Fitness]
                          (moves: GPMoves,
                           cdgpEval: CDGPEvaluation[Op, E],
                           selection: Selection[Op, E])
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





abstract class CDGPSteadyStateLexicaseCore[E <: Fitness]
            (moves: GPMoves,
             cdgpEval: CDGPEvaluationSteadyState[Op, E],
             correct: (Op, E) => Boolean)
            (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
  extends EACore[Op, E](moves, SequentialEval(cdgpEval.eval), correct) with CDGPAlgorithm[Op, E] {
  override def cdgpState = cdgpEval.state
  override def iter = (s: StatePop[(Op, E)]) => (createBreeder(s) andThen
    CallEvery(opt('reportFreq, opt('populationSize, 1000)), report) andThen
    cdgpEval.updatePopulationEvalsAndTests andThen
    updateAfterIteration)(s)
  override def initialize  = super.initialize
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
  override def algorithm =
    (s: StatePop[(Op, E)]) =>  Common.restartLoop(initialize, super.algorithm andThen bsf, correct, it, bsf, opt)(s)
  val bsf = BestSoFar[Op, E](ordering, it)
  def createBreeder(s: StatePop[(Op, E)]): (StatePop[(Op, E)] => StatePop[(Op, E)])  // to be implemented by children
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
                                            correct: (Op, E) => Boolean,
                                            selection: Selection[Op, E],
                                            deselection: Selection[Op, E])
                                           (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
  extends CDGPSteadyStateLexicaseCore[E](moves, cdgpEval, correct) with CDGPAlgorithm[Op, E] {
  val breeder = new SimpleSteadyStateBreeder[Op, E](selection, RandomMultiOperator(moves: _*), deselection, cdgpEval.eval)
  override def createBreeder(s: StatePop[(Op, E)]) = breeder
}


object CDGPSteadyStateLexicase {
  def getSelection()(implicit rng: TRandom): Selection[Op, FSeqInt] =
    new LexicaseSelection01[Op, FSeqInt]

  def getDeselection(eval: EvalFunction[Op, FSeqInt])(implicit opt: Options, rng: TRandom): Selection[Op, FSeqInt] =
    if (opt('lexicaseDeselection, false)) new LexicaseSelection01[Op, FSeqInt]
    else {
      val k = opt('tournamentSize, 7, (_: Int) >= 1)
      new TournamentSelection[Op, FSeqInt](eval.ordering.reverse, k)
    }

  def apply(eval: EvalFunction[Op, FSeqInt])
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyStateLexicase[FSeqInt] = {
    implicit val ordering = eval.ordering
    val grammar = eval.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(eval.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluationSteadyState[Op, FSeqInt](eval, eval.updateEval)
    val correct = Common.correct(cdgpEval.eval)
    val sel = CDGPSteadyStateLexicase.getSelection()
    val desel = CDGPSteadyStateLexicase.getDeselection(eval)
    new CDGPSteadyStateLexicase(moves, cdgpEval, correct, sel, desel)
  }
}



/**
 * CDGP with steady state and eps-lexicase for regression used as a selection. Eps-lexicase
 * requires certain information precomputed from the current population, so it needs to be
 * created for each new generation.
 */
class CDGPSteadyStateLexicaseR[E <: FSeqDouble](moves: GPMoves,
                                                cdgpEval: CDGPEvaluationSteadyState[Op, E],
                                                correct: (Op, E) => Boolean,
                                                deselection: Selection[Op, E])
                                               (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
  extends CDGPSteadyStateLexicaseCore[E](moves, cdgpEval, correct) with CDGPAlgorithm[Op, E] {
  override def initialize: Unit => StatePop[(Op, E)] = {
    Common.addNoiseToTests(cdgpEval.state)(opt, rng); super.initialize
  }
  def createBreeder(s: StatePop[(Op, E)]): (StatePop[(Op, E)] => StatePop[(Op, E)]) = {
    val epsForTests = EpsLexicaseSelection.medianAbsDev(s)
    val sel = new EpsLexicaseSelection[Op, E](epsForTests)
    new SimpleSteadyStateBreeder[Op, E](sel, RandomMultiOperator(moves: _*), deselection, cdgpEval.eval)
  }
}


object CDGPSteadyStateLexicaseR {
  def getDeselection[E <: FSeqDouble](eval: EvalFunction[Op, E])(implicit opt: Options, rng: TRandom): Selection[Op, E] = {
    val k = opt('tournamentSize, 7, (_: Int) >= 1)
    new TournamentSelection[Op, E](eval.ordering.reverse, k)
  }

  def apply[E <: FSeqDouble](eval: EvalFunction[Op, E])
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyStateLexicaseR[E] = {
    implicit val ordering = eval.ordering
    val grammar = eval.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(eval.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluationSteadyState[Op, E](eval, eval.updateEval)
    val correct = Common.correct(cdgpEval.eval)
    val desel = CDGPSteadyStateLexicaseR.getDeselection(eval)
    new CDGPSteadyStateLexicaseR[E](moves, cdgpEval, correct, desel)
  }
}





class CDGPConvectionEqualNumber[S <: Op, E <: Fitness]
      (override val cdgpState: State,
       val evalFunc: EvalFunction[S,E],
       eaCreator: () => EACore[S,E] with CDGPAlgorithm[S,E],
       reportPreDivide: Seq[StatePop[(S, E)]] => Seq[StatePop[(S, E)]] = (s:Seq[StatePop[(S, E)]]) => s,
       reportPostDivide: Seq[StatePop[(S, E)]] => Seq[StatePop[(S, E)]] = (s:Seq[StatePop[(S, E)]]) => s)
      (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
  extends MultipopulationEA[S, E](
    MultipopulationEA.convectionEqualNumber(opt("multipop.M", 5), ordering),
    eaCreator,
    maxIter = Option(opt("multipop.maxGenerations", 100)),
    maxTime = Option(opt("multipop.maxTime", 86400000)),
    stop = (pop: StatePop[(S, E)]) => {pop.exists(_._2.correct)})
  with CDGPAlgorithm[S, E] {

  override def iter: Seq[StatePop[(S,E)]] => Seq[StatePop[(S,E)]] =
    popsEvolve andThen savePop andThen reportPreDivide andThen popsDivide andThen
    reportPostDivide andThen report //andThen printPops

  def savePop(pops: Seq[StatePop[(S, E)]]): Seq[StatePop[(S, E)]] = {
    pop = Some(StatePop(pops.flatten))
    pops
  }

  override def report: Function1[Seq[StatePop[(S, E)]], Seq[StatePop[(S, E)]]] = bsfPops
  override def epilogue: Function1[Seq[StatePop[(S, E)]], Seq[StatePop[(S, E)]]] =
    bsfPops andThen reportStatsPops andThen epiloguePops
  override val bsf = BestSoFar[S, E](ordering, it)


  override def popsEvolve: Seq[StatePop[(S, E)]] => Seq[StatePop[(S, E)]] =
    (pops: Seq[StatePop[(S,E)]]) => pops.map{ pop =>
      val ea = eaCreator()
      // Synchronize fitness to avoid problems
      val synchrPop = StatePop(pop.map { s => evalFunc.updateEval(s) })
      ea.apply(synchrPop)
    }


  def bsfPops: Function1[Seq[StatePop[(S, E)]], Seq[StatePop[(S, E)]]] =
    (pops: Seq[StatePop[(S, E)]]) => {
      pops.foreach(bsf(_))
      pops
    }

  def reportStatsPops: Function1[Seq[StatePop[(S, E)]], Seq[StatePop[(S, E)]]] =
    (pops: Seq[StatePop[(S, E)]]) => {
      pops.foreach(reportStats(_))
      pops
    }

  def epiloguePops: Function1[Seq[StatePop[(S, E)]], Seq[StatePop[(S, E)]]] =
    (pops: Seq[StatePop[(S, E)]]) => {
      coll.set("multipop.totalGenerations", it.count)
      pops
    }
}


object CDGPConvectionEqualNumber {
  def reportAvgsInGroups(keyPopSuffix: String)(implicit coll: Collector): Seq[StatePop[(Op, FInt)]] => Seq[StatePop[(Op, FInt)]] =
    (pops: Seq[StatePop[(Op, FInt)]]) => {
      pops.indices.zip(pops).foreach { case (i, pop) =>
        val numTests = pop.head._2.totalTests.asInstanceOf[Double]
        val listRatios = pop.map{ case (s, e) => (numTests - e.value) / numTests }
        val k = s"multipop.pop${i}${keyPopSuffix}.fitness"
        computeAndSaveStats(k, listRatios)
      }
      pops
    }

  def reportAvgsInGroupsFSeqInt(keyPopSuffix: String)(implicit coll: Collector): Seq[StatePop[(Op, FSeqInt)]] => Seq[StatePop[(Op, FSeqInt)]] =
    (pops: Seq[StatePop[(Op, FSeqInt)]]) => {
      pops.indices.zip(pops).foreach { case (i, pop) =>
        val numTests = pop.head._2.totalTests.asInstanceOf[Double]
        val listRatios = pop.map{ case (s, e) => (numTests - e.sum) / numTests }
        val k = s"multipop.pop${i}${keyPopSuffix}"
        computeAndSaveStats(k, listRatios)
      }
      pops
    }

  protected def computeAndSaveStats(k: String, passedRatios: Seq[Double])(implicit coll: Collector) {
    val avg = passedRatios.sum / passedRatios.size
    val stdDev = Tools.stddev(passedRatios, avg)
    computeAndSaveStats(k, avg, stdDev )
  }

  protected def computeAndSaveStats(k: String, avg: Double, stdDev: Double)(implicit coll: Collector) {
    val roundedAvg = BigDecimal(avg).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
    val roundedStdDev = BigDecimal(stdDev).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.set(k+".fitnessAvg", coll.get(k+".fitnessAvg").getOrElse("") + s"$roundedAvg,")
    coll.set(k+".fitnessStdDev", coll.get(k+".fitnessStdDev").getOrElse("") + s"$roundedStdDev,")
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

  def addNoiseToTests(state: State)(implicit opt: Options, rng: TRandom): Unit = {
    state.testsManager = NoiseAdderStdDev(state.testsManager)
  }

  def restartLoop[S,E](initialize: Unit => StatePop[(S,E)],
                     algorithm: StatePop[(S,E)] => StatePop[(S,E)],
                     correct: (S, E) => Boolean,
                     callCounter: CallCounter[StatePop[(S,E)], StatePop[(S,E)]],
                     bsf: BestSoFar[S, E],
                     opt: Options
                    )(s: StatePop[(S,E)]): StatePop[(S,E)] = {
    @scala.annotation.tailrec
    def helper(startPop: StatePop[(S,E)], m: Int): StatePop[(S,E)] = {
      // println(s"\n----- Algorithm run #${opt('numRestarts, 1)-m} -----")
      if (m == 1) algorithm(startPop)
      else {
        val res = algorithm(startPop)
        val b = bsf.bestSoFar.get
        if (bsf.bestSoFar.isDefined && correct(b._1, b._2)) res
        else {
          callCounter.reset()
          helper(initialize(), m-1)
        }
      }
    }
    helper(s, opt('numRestarts, 1, (x: Int) => x >= 1))
  }
}

package cdgp

import fuel.core.StatePop
import fuel.func._
import fuel.util.{Collector, Options, TRandom}
import swim.eval.{EpsLexicaseSelection, LexicaseSelection01}
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

  /**
    * Saves the provided state of the population and returns it, so that it can
    * be used further in the pipe.
    */
  def saveCurrentPop(s: StatePop[(S, E)]): StatePop[(S, E)] = {
    pop = Some(s)
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
      extends SimpleGP(moves, cdgpEval.eval, Common.correct) with CDGPAlgorithm[Op, E] {
  override def cdgpState = cdgpEval.state
  override def initialize  = super.initialize
  override def epilogue = super.epilogue andThen bsf andThen Common.reportStats(cdgpEval.state, bsf)
  override def iter = super.iter andThen saveCurrentPop // andThen super.report // uncomment report to change the result (FUEL issue #6)
  override def evaluate = cdgpEval
}

object CDGPGenerational {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerational[FInt] = {
    val cdgp = Common.getCDGPState(benchmark)
    val cdgpFit = new CDGPFitnessD(cdgp)
    apply(cdgpFit)
  }

  def apply(cdgpFit: CDGPFitnessD)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerational[FInt] = {
    implicit val ordering = FIntOrdering
    val grammar = cdgpFit.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(cdgpFit.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluation[Op, FInt](cdgpFit.state, Common.evalInt(cdgpFit.fitness))
    new CDGPGenerational(moves, cdgpEval)
  }

  def apply(cdgpFit: CDGPFitnessR)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerational[FSeqDouble] = {
    implicit val ordering = FSeqDoubleOrderingMSE
    val grammar = cdgpFit.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(cdgpFit.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluation[Op, FSeqDouble](cdgpFit.state, Common.evalSeqDouble(cdgpFit.fitness))
    new CDGPGenerational[FSeqDouble](moves, cdgpEval)
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
                      cdgpFit: CDGPFitnessD)
                     (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[FInt])
      extends SteadyStateEA[Op, FInt](moves, cdgpEval.eval,
                                      Common.correctInt,
                                      CDGPSteadyState.getSelection(),
                                      CDGPSteadyState.getDeselection()) with CDGPAlgorithm[Op, FInt] {
  override def cdgpState = cdgpEval.state
  override def initialize  = super.initialize
  override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests andThen saveCurrentPop
  override def epilogue = super.epilogue andThen bsf andThen Common.reportStats(cdgpEval.state, bsf)
  override def report = s => s
  override def evaluate = // used only for the initial population
    (s: StatePop[Op]) => {
      cdgpEval.state.testsManager.flushHelpers()
      if (cdgpEval.state.testsManager.tests.isEmpty)
        Common.evalPopToDefaultFInt(false, 0)(s)
      else
        Common.evalPopNoVerificationFInt(cdgpFit)(s)
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
    val cdgp = Common.getCDGPState(benchmark)
    val cdgpFit = new CDGPFitnessD(cdgp)
    apply(cdgpFit)
  }
  def apply(cdgpFit: CDGPFitnessD)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyState = {
    implicit val ordering = FIntOrdering
    val grammar = cdgpFit.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(cdgpFit.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluationSteadyState[Op, FInt](cdgpFit.state, Common.evalInt(cdgpFit.fitness),
      cdgpFit.updateEvalInt)
    new CDGPSteadyState(moves, cdgpEval, cdgpFit)
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
  override val selection = new LexicaseSelection01[Op, FSeqInt]
  override def cdgpState = cdgpEval.state
  override def initialize  = super.initialize
  override def epilogue = super.epilogue andThen bsf andThen Common.reportStats(cdgpEval.state, bsf)
  override def iter = super.iter andThen super.report andThen saveCurrentPop
  override def evaluate = cdgpEval
}

object CDGPGenerationalLexicase {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerationalLexicase = {
    val cdgp = Common.getCDGPState(benchmark)
    val cdgpFit = new CDGPFitnessD(cdgp)
    apply(cdgpFit)
  }
  def apply(cdgpFit: CDGPFitnessD)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerationalLexicase = {
    implicit val ordering = FSeqIntOrdering
    val grammar = cdgpFit.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(cdgpFit.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluation[Op, FSeqInt](cdgpFit.state, Common.evalSeqInt(cdgpFit.fitness))
    new CDGPGenerationalLexicase(moves, cdgpEval)
  }
}





class CDGPGenerationalLexicaseR(moves: GPMoves,
                                cdgpEval: CDGPEvaluation[Op, FSeqDouble])
                               (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[FSeqDouble])
  extends EACore[Op, FSeqDouble](moves, SequentialEval(cdgpEval.eval), Common.correct)
     with CDGPAlgorithm[Op, FSeqDouble] {
  val bsf = BestSoFar[Op, FSeqDouble](ordering, it)
  override def cdgpState = cdgpEval.state
  override def initialize = super.initialize// andThen Common.printPop
  override def epilogue = super.epilogue andThen bsf andThen Common.reportStats(cdgpEval.state, bsf)
  override def iter = (s: StatePop[(Op, FSeqDouble)]) =>
    (createBreeder(s) andThen evaluate andThen super.report andThen saveCurrentPop)(s)
  override def evaluate = cdgpEval

  def createBreeder(s: StatePop[(Op, FSeqDouble)]): (StatePop[(Op, FSeqDouble)] => StatePop[Op]) = {
    val epsForTests = EpsLexicaseSelection.medianAbsDev(s)
    val sel = new EpsLexicaseSelection[Op, FSeqDouble](epsForTests)
    SimpleBreeder(sel, moves: _*)
  }
}

object CDGPGenerationalLexicaseR {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerationalLexicaseR = {
    val cdgp = Common.getCDGPState(benchmark)
    val cdgpFit = new CDGPFitnessR(cdgp)
    apply(cdgpFit)
  }
  def apply(cdgpFit: CDGPFitnessR)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerationalLexicaseR = {
    implicit val ordering = FSeqDoubleOrderingMSE
    val grammar = cdgpFit.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(cdgpFit.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluation[Op, FSeqDouble](cdgpFit.state, Common.evalSeqDouble(cdgpFit.fitness))
    new CDGPGenerationalLexicaseR(moves, cdgpEval)
  }
}





class CDGPGenerationalCore[E <: Fitness]
                          (selection: Selection[Op, E],
                           moves: GPMoves,
                           cdgpEval: CDGPEvaluation[Op, E])
                          (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
  extends EACore[Op, E](moves, SequentialEval(cdgpEval.eval), Common.correct)
     with CDGPAlgorithm[Op, E] {

  override def cdgpState = cdgpEval.state
  val bsf = BestSoFar[Op, E](ordering, it)
  override def iter = SimpleBreeder(selection, moves: _*) andThen evaluate

  override def initialize  = super.initialize
  override def epilogue = super.epilogue andThen bsf andThen Common.reportStats(cdgpEval.state, bsf)
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
class CDGPSteadyStateLexicase(moves: GPMoves,
                              cdgpEval: CDGPEvaluationSteadyState[Op, FSeqInt],
                              cdgpFit: CDGPFitnessD)
                             (implicit opt: Options, coll: Collector, rng: TRandom,
                              ordering: Ordering[FSeqInt])
      extends SteadyStateEA[Op, FSeqInt](moves, cdgpEval.eval,
                                         Common.correctSeqInt,
                                         CDGPSteadyStateLexicase.getSelection(),
                                         CDGPSteadyStateLexicase.getDeselection()) with CDGPAlgorithm[Op, FSeqInt] {
  override def cdgpState = cdgpEval.state
  override def initialize  = super.initialize
  override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests andThen saveCurrentPop
  override def epilogue = super.epilogue andThen bsf andThen Common.reportStats(cdgpEval.state, bsf)
  override def evaluate = // used only for the initial population
    (s: StatePop[Op]) => {
      cdgpEval.state.testsManager.flushHelpers()
      if (cdgpEval.state.testsManager.tests.isEmpty)
        Common.evalPopToDefaultFSeqInt(false, List())(s)
      else
        Common.evalPopNoVerificationFSeqInt(cdgpFit)(s)
    }
}

object CDGPSteadyStateLexicase {
  def getSelection()(implicit rng: TRandom): Selection[Op, FSeqInt] =
    new LexicaseSelection01[Op, FSeqInt]

  def getDeselection()(implicit opt: Options, rng: TRandom): Selection[Op, FSeqInt] =
    if (opt('lexicaseDeselection, false)) new LexicaseSelection01[Op, FSeqInt]
    else {
      val k = opt('tournamentSize, 7, (_: Int) >= 2)
      new TournamentSelection[Op, FSeqInt](FSeqIntOrdering.reverse, opt('tournamentDeselectSize, k, (_: Int) >= 2))
    }

  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyStateLexicase = {
    val cdgp = Common.getCDGPState(benchmark)
    val cdgpFit = new CDGPFitnessD(cdgp)
    apply(cdgpFit)
  }
  def apply(cdgpFit: CDGPFitnessD)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyStateLexicase = {
    implicit val ordering = FSeqIntOrdering
    val grammar = cdgpFit.state.sygusData.getSwimGrammar(rng)
    val moves = GPMoves(grammar, Common.isFeasible(cdgpFit.state.synthTask.fname, opt))
    val cdgpEval = new CDGPEvaluationSteadyState[Op, FSeqInt](cdgpFit.state, Common.evalSeqInt(cdgpFit.fitness),
      cdgpFit.updateEvalSeqInt)
    new CDGPSteadyStateLexicase(moves, cdgpEval, cdgpFit)
  }
}




object Common {
  def correct: (Any, Fitness) => Boolean =
    (_: Any, e: Fitness) => e.correct
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
    FInt(isPerfect, r, s.size)
  }
  def evalSeqInt(fitness: (Op) => (Boolean, Seq[Int]))(s: Op): FSeqInt = {
    val (isPerfect, r) = fitness(s)
    FSeqInt(isPerfect, r, s.size)
  }
  def evalSeqDouble(fitness: (Op) => (Boolean, Seq[Double]))(s: Op): FSeqDouble = {
    val (isPerfect, r) = fitness(s)
    FSeqDouble(isPerfect, r, s.size)
  }

  def evalPopNoVerificationFSeqInt(state: CDGPFitnessD)(s: StatePop[Op]): StatePop[(Op, FSeqInt)] = {
    StatePop(s.map { op =>
      val f = state.fitnessNoVerification(op)
      (op, FSeqInt(f._1, f._2, op.size))
    })
  }
  def evalPopToDefaultFSeqInt(dec: Boolean, fit: Seq[Int])(s: StatePop[Op]): StatePop[(Op, FSeqInt)] = {
    StatePop(s.map{ op => (op, FSeqInt(dec, fit, op.size))})
  }

  def evalPopNoVerificationFInt(state: CDGPFitnessD)(s: StatePop[Op]): StatePop[(Op, FInt)] = {
    StatePop(s.map{ op =>
      val f = state.fitnessNoVerification(op)
      (op, FInt(f._1, f._2, op.size))
    })
  }
  def evalPopToDefaultFInt(dec: Boolean, fit: Int, totalTests: Int = 0)(s: StatePop[Op]): StatePop[(Op, FInt)] = {
    StatePop(s.map{ op => (op, FInt(dec, fit, op.size, totalTests))})
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
    CDGPState(LoadSygusBenchmark(benchmark))
  }

  def reportStats[E <: Fitness](cdgpState: CDGPState, bsf: BestSoFar[Op, E])
                    (s: StatePop[(Op, E)])
                    (implicit opt: Options, coll: Collector): StatePop[(Op, E)] = {
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
    }
    cdgpState.reportData()
    s
  }
}


trait Fitness {
  def correct: Boolean
  /**
    * Saves all the fitness-relevant data using the provided collector.
    */
  def saveInColl(coll: Collector): Unit
}


case class FSeqInt(correct: Boolean, value: Seq[Int], progSize: Int)
  extends Seq[Int] with Fitness {
  override def length: Int = value.length
  override def apply(idx: Int) = value(idx)
  override def iterator = value.iterator

  override def saveInColl(coll: Collector): Unit = {
    val passedTests = if (correct) this.size else this.count(_ == 0)
    val ratio = if (this.isEmpty) 1.0 else passedTests.toDouble / this.size
    val roundedRatio = BigDecimal(ratio).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.setResult("best.passedTests", passedTests)
    coll.setResult("best.numTests", this.size)
    coll.setResult("best.passedTestsRatio", roundedRatio)
    coll.setResult("best.isOptimal", correct)
  }
  override def toString: String = s"Fit($correct, $value, progSize=$progSize)"
}


case class FSeqDouble(correct: Boolean, value: Seq[Double], progSize: Int)
  extends Seq[Double] with Fitness {
  override def length: Int = value.length
  override def apply(idx: Int) = value(idx)
  override def iterator = value.iterator

  lazy val mse: Double = if (value.isEmpty) 0.0 else value.map{ x => x*x }.sum / value.size.toDouble

  override def saveInColl(coll: Collector): Unit = {
    val mseRound = BigDecimal(mse).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.setResult("best.mse", mse)
    coll.setResult("best.isOptimal", correct)
  }
  override def toString: String = s"Fit($correct, $value, progSize=$progSize)"
}


case class FInt(correct: Boolean, value: Int, progSize: Int, totalTests: Int) extends Fitness {
  override def saveInColl(coll: Collector): Unit = {
    val passedTests = if (correct) totalTests else totalTests - value
    val ratio = if (totalTests == 0) 1.0 else passedTests.toDouble / totalTests
    val roundedRatio = BigDecimal(ratio).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.setResult("best.passedTests", passedTests)
    coll.setResult("best.numTests", totalTests)
    coll.setResult("best.passedTestsRatio", roundedRatio)
    coll.setResult("best.isOptimal", correct)
  }
  override def toString: String = s"Fit($correct, $value, progSize=$progSize)"
}
object FInt {
  def apply(correct: Boolean, list: Seq[Int], progSize: Int): FInt =
    FInt(correct, list.sum, progSize, list.size)
}


case class FitnessMSE(correct: Boolean, value: Double, progSize: Int) extends Fitness {
  override def saveInColl(coll: Collector): Unit = {
    val mse = BigDecimal(value).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.setResult("best.mse", mse)
    coll.setResult("best.isOptimal", correct)
  }
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
object FSeqDoubleOrderingMSE extends Ordering[FSeqDouble] {
  override def compare(a: FSeqDouble, b: FSeqDouble): Int = {
    val c = if (a.correct && !b.correct) -1
    else if (!a.correct && b.correct) 1
    else a.mse.compareTo(b.mse)
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
object FitnessMSEOrdering extends Ordering[FitnessMSE] {
  override def compare(a: FitnessMSE, b: FitnessMSE): Int = {
    val c = if (a.correct && !b.correct) -1
    else if (!a.correct && b.correct) 1
    else a.value compare b.value
    // lexicographic parsimony pressure
    if (c == 0) a.progSize compare b.progSize
    else c
  }
}
package sygusgp

import fuel.core.StatePop
import fuel.func._
import fuel.util.{Collector, Options, TRandom}
import swim.eval.LexicaseSelection
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
                       cdgpEval: CDGPEvaluation[Op, Int])
                      (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[Int])
      extends SimpleGP(moves, cdgpEval.eval, Common.correctInt) {
  override def epilogue = super.epilogue andThen bsf andThen Common.epilogueEvalInt(cdgpEval.state, bsf)
  override def iter = super.iter// andThen super.report // uncomment report to change the result (FUEL issue #6)
  override def evaluate = cdgpEval
}

object CDGPGenerational {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerational = {
    val cdgpState = Common.getCDGPState(benchmark)
    val moves = GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible)
    val cdgpEval = new CDGPEvaluation(cdgpState, Common.evalInt(cdgpState.fitness))
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
                      cdgpEval: CDGPEvaluationSteadyState[Op, Int])
                     (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[Int])
      extends SimpleSteadyStateEA[Op, Int](moves, cdgpEval.eval, Common.correctInt) {
  override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests andThen Common.printPop
  override def epilogue = super.epilogue andThen bsf andThen Common.epilogueEvalInt(cdgpEval.state, bsf)
  override def evaluate = cdgpEval
  override def report = s => s
}

object CDGPSteadyState {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyState = {
    val cdgpState = Common.getCDGPState(benchmark)
    val moves = GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible)
    val cdgpEval = new CDGPEvaluationSteadyState(cdgpState, Common.evalInt(cdgpState.fitness),
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
                               cdgpEval: CDGPEvaluation[Op, Seq[Int]])
                              (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[Int])
      extends LexicaseGP(moves, cdgpEval.eval, Common.correctSeqInt) {
  override def epilogue = super.epilogue andThen bsf andThen Common.epilogueEvalSeqInt(cdgpEval.state, bsf)
  override def iter = super.iter andThen super.report// andThen Common.printPop
  override def evaluate = cdgpEval
}

object CDGPGenerationalLexicase {
  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPGenerationalLexicase = {
    val cdgpState = Common.getCDGPState(benchmark)
    val moves = GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible)
    val cdgpEval = new CDGPEvaluation(cdgpState, Common.evalSeqInt(cdgpState.fitness))
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
                              cdgpEval: CDGPEvaluationSteadyState[Op, Seq[Int]])
                             (implicit opt: Options, coll: Collector, rng: TRandom,
                              ordering: Ordering[Seq[Int]] = LongerOrMaxPassedOrdering)
      extends SteadyStateEA[Op, Seq[Int]](moves, cdgpEval.eval,
                                          Common.correctSeqInt,
                                          CDGPSteadyStateLexicase.getSelection(),
                                          CDGPSteadyStateLexicase.getDeselection()) {
  override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests
  override def epilogue = super.epilogue andThen bsf andThen Common.epilogueEvalSeqInt(cdgpEval.state, bsf)
  override def evaluate = cdgpEval
}

object CDGPSteadyStateLexicase {
  def getSelection()(implicit rng: TRandom): Selection[Op, Seq[Int]] = new LexicaseSelection[Op, Int](Ordering[Int])

  def getDeselection()(implicit opt: Options, rng: TRandom): Selection[Op, Seq[Int]] =
    if (opt('lexicaseDeselection, false)) new LexicaseSelection[Op, Int](Ordering[Int].reverse)
    else new TournamentSelection[Op, Seq[Int]](LongerOrMaxPassedOrdering.reverse)

  def apply(benchmark: String)
           (implicit opt: Options, coll: Collector, rng: TRandom): CDGPSteadyStateLexicase = {
    val cdgpState = Common.getCDGPState(benchmark)
    val moves = GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible)
    val cdgpEval = new CDGPEvaluationSteadyState(cdgpState, Common.evalSeqInt(cdgpState.fitness),
      cdgpState.updateEvalSeqInt)
    new CDGPSteadyStateLexicase(moves, cdgpEval)
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

  def epilogueEvalInt(cdgpState: CDGPState, bsf: BestSoFar[Op, Int])
                     (s: StatePop[(Op, Int)])
                     (implicit opt: Options, coll: Collector): StatePop[(Op, Int)] = {
    val (_, e) = bsf.bestSoFar.get
    val totalTests = cdgpState.testsManager.getNumberOfTests.toDouble
    val passedTests = if (e <= 0) totalTests else totalTests - e
    val ratio = passedTests / totalTests
    val roundedRatio = BigDecimal(ratio).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    val isOptimal = e == -1
    coll.setResult("best.passedTests", passedTests)
    coll.setResult("best.passedTestsRatio", roundedRatio)
    coll.setResult("best.isOptimal", isOptimal)
    coll.setResult("best.isApproximate", !isOptimal)
    reportStats(cdgpState, bsf)(s)
  }

  def epilogueEvalSeqInt(cdgpState: CDGPState, bsf: BestSoFar[Op, Seq[Int]])
                        (s: StatePop[(Op, Seq[Int])])
                        (implicit opt: Options, coll: Collector): StatePop[(Op, Seq[Int])] = {
    val (_, e) = bsf.bestSoFar.get
    val passedTests = if (e.head == -1) cdgpState.testsManager.tests.size else e.count(_ == 0)
    val ratio = if (e.isEmpty || e.head == -1) 1.0 else passedTests / e.size.toDouble
    val roundedRatio = BigDecimal(ratio).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    val isOptimal = e.head == -1
    coll.setResult("best.passedTests", passedTests)
    coll.setResult("best.passedTestsRatio", roundedRatio)
    coll.setResult("best.isOptimal", isOptimal)
    coll.setResult("best.isApproximate", !isOptimal)
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
    // cdgp.totalTests can be higher then best.passedTests for optimal solution,
    // because newly created tests are added before checking correctness of the solution.
    coll.set("cdgp.totalTests", cdgpState.testsManager.tests.size)
    coll.set("cdgp.totalTestsKnownOutputs", cdgpState.testsManager.getNumberOfKnownOutputs)
    coll.set("cdgp.totalTestsUnknownOutputs", cdgpState.testsManager.getNumberOfUnknownOutputs)
    coll.set("cdgp.totalSolverCalls", cdgpState.solver.getNumCalls)
    coll.set("cdgp.totalSolverRestarts", cdgpState.solver.getNumRestarts)
    coll.saveSnapshot("cdgp")
    s
  }
}
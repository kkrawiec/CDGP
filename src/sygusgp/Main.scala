package sygusgp

import fuel.core.StatePop
import fuel.func._
import fuel.util.FApp
import swim.eval.LexicaseSelection
import swim.tree._



/**
  * Entry point to run various pre-built variants of CDGP.
  *
  * Obligatory options:
  * --searchAlgorithm, accepted values: GP, GPSteadyState, Lexicase, LexicaseSteadyState
  * --benchmark, path to the SyGuS benchmark
  * --solverPath, path to the SMT solver (e.g. Z3).
  */
object Main extends FApp {
  val cdgpState = CDGPState(opt('benchmark))

  val (res, bestOfRun) = opt('searchAlgorithm) match {
    case "GP" => {
      // Convention: an ideal program has fitness -1
//      def evalGP(s: Op): Int = {
//        val (isPerfect, r) = cdgpState.fitness(s)
//        if (isPerfect) -1 // perfect program found; end of run
//        else r.sum
//      }
//      def correct = (_: Any, e: Int) => e == -1
//      val alg = new SimpleGP(GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible), evalGP, correct) {
//
//        // In our scenario there is no meaning in comparing past best solutions with the current ones,
//        // because the current ones have been most likely evaluated on the different set of tests.
//        override def epilogue = super.epilogue andThen bsf// andThen reportStats(bsf) andThen epilogueGP(bsf)
//
//        override def report: (StatePop[(Op, Int)]) => StatePop[(Op, Int)] =
//          s => s  // no standard reporting
//
//        override def iter = super.iter andThen printPop
//
//        override def evaluate: Evaluation[Op, Int] =
//          new CDGPEvaluation(cdgpState.testsManager, evalGP)
//      }
      val alg = CDGPGenerationalGP(opt('benchmark))
      val finalPop = RunExperiment(alg)
      (finalPop, alg.bsf.bestSoFar)
    }


    case "GPSteadyState" => {
      // Convention: an ideal program has fitness -1
//      def evalGP(s: Op): Int = {
//        val (isPerfect, r) = cdgpState.fitness(s)
//        if (isPerfect) -1 // perfect program found; end of run
//        else r.sum
//      }
//      def correct = (_: Any, e: Int) => e == -1
//      val alg = new SimpleSteadyStateEA[Op, Int](GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible), evalGP, correct) {
//        val cdgpEval = new CDGPEvaluationSteadyState[Op, Int](cdgpState.testsManager,
//          evalGP, cdgpState.updateEvalInt)
//
//        override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests
//        override def epilogue = super.epilogue andThen bsf// andThen reportStats(bsf) andThen epilogueGP(bsf)
//        override def report = s => s
//        override def evaluate: Evaluation[Op, Int] = cdgpEval
//      }
      val alg = CDGPSteadyStateGP(opt('benchmark))
      val finalPop = RunExperiment(alg)
      val bestSoFar: Option[(Op, Int)] = alg.bsf.bestSoFar
      (finalPop, bestSoFar)
    }


    case "Lexicase" => {
      // Convention: an ideal program has all test outcomes == -1
      def eval(s: Op): Seq[Int] = {
        val (isPerfect, r) = cdgpState.fitness(s)
        if (isPerfect) r.map(_ => -1) // perfect program found; end of run
        else r
      }
      def correct = (_: Any, e: Seq[Int]) => e.nonEmpty && e.head == -1
      val alg = new LexicaseGP(GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible), eval, correct) {
        override def iter = super.iter
        override def epilogue = super.epilogue andThen bsf// andThen reportStats(bsf) andThen epilogueLexicase(bsf)
        override def report = s => s
        override def evaluate: Evaluation[Op, Seq[Int]] =
          new CDGPEvaluation(cdgpState, eval)
      }
      val finalPop = RunExperiment(alg)
      (finalPop, alg.bsf.bestSoFar)
    }


    case "LexicaseSteadyState" => {
      // Convention: an ideal program has all test outcomes == -1
      def eval(s: Op): Seq[Int] = {
        val (isPerfect, r) = cdgpState.fitness(s)
        if (isPerfect) r.map(_ => -1) // perfect program found; end of run
        else r
      }
      def correct = (_: Any, e: Seq[Int]) => e.nonEmpty && e.head == -1
      val selection = new LexicaseSelection[Op, Int](Ordering[Int])
      val deselection =
        if (opt('lexicaseDeselection, false)) new LexicaseSelection[Op, Int](Ordering[Int].reverse)
        else new TournamentSelection[Op, Seq[Int]](LongerOrMaxPassedOrdering.reverse)
      implicit val ordering = LongerOrMaxPassedOrdering
      val alg = new SteadyStateEA[Op, Seq[Int]](GPMoves(cdgpState.grammar, SimpleGP.defaultFeasible), eval, correct, selection, deselection) {
        val cdgpEval = new CDGPEvaluationSteadyState[Op, Seq[Int]](cdgpState,
          eval, cdgpState.updateEvalSeqInt)

        override def iter = super.iter andThen cdgpEval.updatePopulationEvalsAndTests
        override def epilogue = super.epilogue andThen bsf// andThen reportStats(bsf) andThen epilogueLexicase(bsf)
        override def report = s => s
        override def evaluate: Evaluation[Op, Seq[Int]] = cdgpEval
      }
      val finalPop = RunExperiment(alg)
      (finalPop, alg.bsf.bestSoFar)
    }
  }





  def isOptimal(bestOfRun: (Op, Any)): Boolean = {
    bestOfRun._2 match {
      case a if a.isInstanceOf[Int]      => a.asInstanceOf[Int] == -1
      case a if a.isInstanceOf[Seq[Int]] => a.asInstanceOf[Seq[Int]].nonEmpty && a.asInstanceOf[Seq[Int]].head == -1
    }
  }


  val passedTestsRatio = coll.getResult("best.passedTestsRatio").getOrElse("n/a")
  println("\nBest program found: " + coll.getResult("best").getOrElse("n/a"))
  println("Evaluation: " + coll.getResult("best.eval").getOrElse("n/a"))
  println("Ratio of passed tests: " + passedTestsRatio)
  println("Total solver calls: " + cdgpState.solver.getNumCalls)
  println("Total time [ms]: " + coll.getResult("totalTimeSystem").getOrElse("Unknown"))
  //println("Total tests: " + testsManager.tests.size)


  assume(bestOfRun.isDefined, "No solution (optimal or approximate) to the problem was found.")
  val solutionCode = SMTLIBFormatter.synthTaskSolutionToString(cdgpState.synthTask, bestOfRun.get._1)

  println("\nOPTIMAL SOLUTION:")
  if (isOptimal(bestOfRun.get)) println(solutionCode) else println("unknown")

  if (!isOptimal(bestOfRun.get)) {
    println(f"\nAPPROXIMATED SOLUTION:\n(passedTestsRatio $passedTestsRatio)")
    println(solutionCode)
  }
}

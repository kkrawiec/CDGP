package sygusgp

import fuel.core.StatePop
import fuel.func.{Evaluation, RunExperiment}
import fuel.util.FApp
import swim.tree.{GPMoves, LexicaseGP, Op, SimpleGP}


object Main extends FApp {
  val benchmark = opt('benchmark)
  val sygusProblem = LoadSygusBenchmark(benchmark)
  // Retrieve the grammar and signature of the function to be synthesized
  val synthTasks: Seq[SygusSynthesisTask] = ExtractSynthesisTasks(sygusProblem)
  if (synthTasks.size > 1)
    throw new Exception("SKIPPING: Multiple synth-fun commands detected. Cannot handle such problems.")
  val synthTask = synthTasks.head
  val grammar = ExtractSygusGrammar(synthTask)

  // Creating solver manager
  val solverPath = opt('solverPath)
  val solverArgs = opt('solverArgs, "-in")
  val solver = new SolverManager(solverPath, solverArgs, verbose=false)

  val cdgpFactory = new CGDPFitness(sygusProblem)

  val searchAlg = opt('searchAlgorithm, "")
  assume(searchAlg == "GP" || searchAlg == "GPSteadyState" || searchAlg == "Lexicase" || searchAlg == "LexicaseSteadyState")
  val method = opt('method, "CDGP")
  assume(method == "CDGP" || method == "CDGPcons" || method == "GPR")
  val fitness = method match {
    case "CDGP"     => cdgpFactory.fitnessCDGP
    case "CDGPcons" => cdgpFactory.fitnessCDGPConservative
//    case "GPR"      => cdgpFactory.fitnessGPR
  }


  def printPop[E](s: StatePop[(Op, E)]): StatePop[(Op, E)] = {
    println(f"\nPopulation (size=${s.size}):")
    for (x <- s)
      println(x)
    println()
    s
  }



  val (res, bestOfRun) = searchAlg match {
    case "GP" => {
      // Convention: an ideal program has fitness -1.
      def evalGP(s: Op): Int = {
        val (isPerfect, r) = fitness(s)
        if (isPerfect) -1 // perfect program found; end of run
        else r.sum
      }
      def correct = (_: Any, e: Int) => e == -1
      val alg = new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible), evalGP, correct) {

        // In our scenario there is no meaning in comparing past best solutions with the current ones,
        // because the current ones have been most likely evaluated on the different set of tests.
        override def epilogue = super.epilogue andThen bsf// andThen reportStats(bsf) andThen epilogueGP(bsf)

        override def report: (StatePop[(Op, Int)]) => StatePop[(Op, Int)] =
          s => s  // no standard reporting

        override def iter = super.iter andThen printPop

        override def evaluate: Evaluation[Op, Int] =
          new CDGPEvaluation(cdgpFactory.testsManager, fitness, evalGP, cdgpFactory.updateEvalInt)
      }
      val finalPop = RunExperiment(alg)
      (finalPop, alg.bsf.bestSoFar)
    }


    case "Lexicase" => {
      // Convention: an ideal program has all test outcomes == -1 (but it's enough to check the first one)
      def eval(s: Op): Seq[Int] = {
        val (isPerfect, r) = fitness(s)
        if (isPerfect) r.map(_ => -1) // perfect program found; end of run
        else r
      }
      def correct = (_: Any, e: Seq[Int]) => e.nonEmpty && e.head == -1
      val alg = new LexicaseGP(GPMoves(grammar, SimpleGP.defaultFeasible), eval, correct) {
        override def iter = super.iter
        override def epilogue = super.epilogue andThen bsf// andThen reportStats(bsf) andThen epilogueLexicase(bsf)
        override def report = s => s
        override def evaluate: Evaluation[Op, Seq[Int]] =
          new CDGPEvaluation(cdgpFactory.testsManager, fitness, eval, cdgpFactory.updateEvalSeqInt)
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

  println("\nBest program found: " + coll.getResult("best").getOrElse("None"))
  println("Evaluation: " + coll.getResult("best.eval").getOrElse("None"))
  println("Ratio of passed tests: " + coll.getResult("best.passedTestsRatio").getOrElse("None"))
  println("Total solver calls: " + solver.getNumCalls)
  println("Total time [ms]: " + coll.getResult("totalTimeSystem").getOrElse("Unknown"))
  //println("Total tests: " + testsManager.tests.size)
  def printInSygusFormat(bestOfRun: Option[(Op, Any)]) {
    if (bestOfRun.isDefined) {
      val (best, _) = bestOfRun.get
      if (isOptimal(bestOfRun.get)) {
        val bestBody = SMTLIBFormatter.opToString(best)
        val args = SMTLIBFormatter.synthFunArgsToString(synthTask)
        val tpe = SMTLIBFormatter.sortToString(synthTask.outputType)
        println(f"(define-fun ${synthTask.fname} ($args) $tpe\n\t$bestBody)")
      }
      else
        println("unknown")
    }
    else
      println("unknown")
  }

  println("\nOPTIMAL SOLUTION:")
  printInSygusFormat(bestOfRun)
}

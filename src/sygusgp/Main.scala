package sygusgp

import fuel.func.RunExperiment
import fuel.util.FApp
import swim.tree.{GPMoves, Op, SimpleGP}


class Main extends FApp {
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

  val (res, bestOfRun) = searchAlg match {
    case "GP" => {
      // Convention: an ideal program has fitness -1.
      def evalGP(s: Op): Int = {
        val r = fitness(s)
        if (r.isLeft) -1 // perfect program found; end of run
        else r.right.get.sum
      }
      val cdgpEval = new CDGPEvaluation(cdgpFactory.testsManager, fitness, evalGP)
      def correct = (_: Any, e: Int) => e == -1
      val alg = new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible), evalGP, correct) {
        // In our scenario there is no meaning in comparing past best solutions with the current ones,
        // because the current ones have been most likely evaluated on the different set of tests.
        override def epilogue = super.epilogue andThen bsf// andThen reportStats(bsf) andThen epilogueGP(bsf)

        override def report = s => s

        //override def evaluate = cdgpEval
      }
      val finalPop = RunExperiment(alg)
      val bestSoFar: Option[(Op, Int)] = alg.bsf.bestSoFar
      (finalPop, bestSoFar)
    }
  }
}

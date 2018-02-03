package app

import java.io.{File, PrintWriter, StringWriter}

import fuel.func.RunExperiment
import fuel.core.StatePop
import fuel.util._
import swim.tree.Op
import cdgp.{Fitness, _}

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


/**
  * Entry point to run various pre-built variants of CDGP.
  *
  * Obligatory options:
  * --benchmark, path to the SyGuS benchmark
  * --solverPath, path to the SMT solver (e.g. Z3)
  *
  * Main evolution options:
  * --selection, selection algorithm. Values: lexicase, tournament
  * --evolutionMode, Values: generational, steadyState
  */
object Main {

  def runWithTimeout[T](timeoutMs: Long)(f: => T) : T = {
    Await.result(Future(f), Duration(timeoutMs, MILLISECONDS))
  }


  def getOptions(args: Array[String]): Options = {
    val opt = Options(args)
    if (opt.getOption("optionsFile").isDefined) {
      println("Options loaded from file: " + opt.paramString("optionsFile"))
      val tmp = Options.loadFromFile(new File(opt.getOption("optionsFile").get))
      new OptionsMap(tmp.allOptions.filter{ case (k, v) => !k.startsWith("result") })
    }
    else opt
  }


  def watchTime[E <: Fitness](alg: CDGPAlgorithm[Op, E], f: => Option[StatePop[(Op, E)]])
                             (implicit coll: Collector, opt: Options): Option[StatePop[(Op, E)]] = {
    val maxTime = opt('maxTime, 86400000)  // 24h in miliseconds
    try {
      val res = runWithTimeout(maxTime)(f)
      res
    }
    catch {
      case e: java.util.concurrent.TimeoutException =>
        println("Timeout!!!!!!!!!!!!!!!!!!!")
        coll.set("cdgp.wasTimeout", true)
        coll.set("result.totalTimeSystem", maxTime)  // save in ms
        if (alg.pop.isDefined) {
          alg.bsf(alg.pop.get) // update bsf
          Common.reportStats(alg.cdgpState, alg.bsf)(alg.pop.get)
        }
        coll.saveSnapshot("cdgp")
        alg.pop
    }
  }



  def runConfigGPR(benchmark: String, selection: String, evoMode: String)
                  (implicit coll: Collector, opt: Options, rng: TRandom):
  (StateCDGP, Option[StatePop[(Op, Fitness)]], Option[(Op, Fitness)]) = {
    val state = StateGPR(benchmark)
    (selection, evoMode) match {
      case ("tournament", "generational") =>
        val eval = new EvalGPRInt(state)
        val alg = CDGPGenerational(eval)
        val finalPop = watchTime(alg, RunExperiment(alg))
        (state, finalPop, alg.bsf.bestSoFar)

      case ("tournament", "steadyState") =>
        val eval = new EvalGPRInt(state)
        val alg = CDGPSteadyState(eval)
        val finalPop = watchTime(alg, RunExperiment(alg))
        (state, finalPop, alg.bsf.bestSoFar)

      case ("lexicase", "generational") =>
        val eval = new EvalGPRSeqInt(state)
        val alg = CDGPGenerationalLexicase(eval)
        val finalPop = watchTime(alg, RunExperiment(alg))
        (state, finalPop, alg.bsf.bestSoFar)

      case ("lexicase", "steadyState") =>
        val eval = new EvalGPRSeqInt(state)
        val alg = CDGPSteadyStateLexicase(eval)
        val finalPop = watchTime(alg, RunExperiment(alg))
        (state, finalPop, alg.bsf.bestSoFar)
    }
  }



  def runConfigCDGP(benchmark: String, selection: String, evoMode: String)
                   (implicit coll: Collector, opt: Options, rng: TRandom):
    (StateCDGP, Option[StatePop[(Op, Fitness)]], Option[(Op, Fitness)]) = {
    val state = StateCDGP(benchmark)
    (selection, evoMode) match {
      case ("tournament", "generational") =>
        val eval = new EvalCDGPInt(state)
        val alg = CDGPGenerational(eval)
        val finalPop = watchTime(alg, RunExperiment(alg))
        (state, finalPop, alg.bsf.bestSoFar)

      case ("tournament", "steadyState") =>
        val eval = new EvalCDGPInt(state)
        val alg = CDGPSteadyState(eval)
        val finalPop = watchTime(alg, RunExperiment(alg))
        (state, finalPop, alg.bsf.bestSoFar)

      case ("lexicase", "generational") =>
        val eval = new EvalCDGPSeqInt(state)
        val alg = CDGPGenerationalLexicase(eval)
        val finalPop = watchTime(alg, RunExperiment(alg))
        (state, finalPop, alg.bsf.bestSoFar)

      case ("lexicase", "steadyState") =>
        val eval = new EvalCDGPSeqInt(state)
        val alg = CDGPSteadyStateLexicase(eval)
        val finalPop = watchTime(alg, RunExperiment(alg))
        (state, finalPop, alg.bsf.bestSoFar)
    }
  }


  def printResults(cdgpState: State, bestOfRun: Option[(Op, Fitness)])
                  (implicit coll: Collector, opt: Options, rng: TRandom) {
    assume(bestOfRun.isDefined, "No solution (optimal or approximate) to the problem was found.")
    def isOptimal(bestOfRun: (Op, Fitness)): Boolean = bestOfRun._2.correct

    val passedTestsRatio = coll.getResult("best.passedTestsRatio").getOrElse("n/a")
    val pn = 26
    println("\n")
    println("Best program found:".padTo(pn, ' ') + coll.getResult("bestOrig.smtlib").getOrElse("n/a"))
    println("Simplified:".padTo(pn, ' ') + coll.getResult("best.smtlib").getOrElse("n/a"))
    println("Evaluation:".padTo(pn, ' ') + coll.getResult("best.eval").getOrElse("n/a"))
    println("Program size:".padTo(pn, ' ') + coll.getResult("best.size").getOrElse("n/a"))
    println("Ratio of passed tests:".padTo(pn, ' ') + passedTestsRatio)
    println("Tests total:".padTo(pn, ' ') + coll.get("tests.total").getOrElse("n/a"))
    println("Tests known outputs:".padTo(pn, ' ') + coll.get("tests.totalKnownOutputs").getOrElse("n/a"))
    println("Total solver calls:".padTo(pn, ' ') + coll.get("solver.totalCalls").getOrElse("n/a"))
    println("Generations:".padTo(pn, ' ') + coll.getResult("best.generation").getOrElse("n/a"))
    println("Total time [s]:".padTo(pn, ' ') + coll.getResult("totalTimeSystem").get.toString.toDouble / 1000.0)
    println("Log file:".padTo(pn, ' ') + coll.get("thisFileName").getOrElse("n/a"))

    if (opt("printTests", false)) {
      println("\nCollected tests:")
      cdgpState.testsManager.tests.foreach(println(_))
      println("")
    }

    val sol = coll.getResult("best.smtlib").get.toString
    val solutionFull = SMTLIBFormatter.synthSolutionToString(cdgpState.synthTask, sol)

    println("\nOPTIMAL SOLUTION:")
    if (isOptimal(bestOfRun.get))
      println(solutionFull) else println("unknown")

    if (!isOptimal(bestOfRun.get)) {
      println(s"\nAPPROXIMATED SOLUTION:\n(passedTestsRatio $passedTestsRatio)")
      println(solutionFull)
    }
  }


  def run(implicit opt: Options): Unit = {
    implicit val coll = CollectorFile(opt)
    implicit val rng = Rng(opt)

    try {
      val benchmark = opt('benchmark)
      println(s"Benchmark: $benchmark")

      val method = opt('method, "CDGP")
      val selection = opt('selection, "lexicase")
      val evoMode = opt('evolutionMode, "generational")
      assert(method == "CDGP" || method == "GPR", s"Invalid method '$method'! Possible values: 'CDGP', 'GPR'.")
      assert(evoMode == "generational" || evoMode == "steadyState",
        s"Invalid evolutionMode: '$evoMode'! Possible values: 'generational', 'steadyState'.")
      assert(selection == "tournament" || selection == "lexicase",
        s"Invalid selection: '$selection'! Possible values: 'tournament', 'lexicase'.")


      // Run algorithm
      val (state, _, bestOfRun) =
        if (method == "CDGP")
          runConfigCDGP(benchmark, selection, evoMode)
        else
          runConfigGPR(benchmark, selection, evoMode)


      // Print and save results
      coll.saveSnapshot("cdgp")
      printResults(state, bestOfRun)
    }
    catch {
      case e: NoSolutionException =>
        println(s"There is no solution to this problem.")
        println(s"Input with no correct answer: " + e.badInput)
        coll.set("cdgp.noCorrectSolution", true)
        coll.set("terminatingException", e.toString)
        coll.saveSnapshot("cdgp")
      case e: java.util.concurrent.TimeoutException =>
        println("Timeout!!!!!!!!!!!!!!!!!!!")
        coll.set("cdgp.wasTimeout", true)
        coll.saveSnapshot("cdgp")
      case e: Throwable =>
        println(s"Terminating exception occurred! Message: ${e.getMessage}")
        coll.set("terminatingException", e.toString)
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        coll.set("terminatingExceptionStacktrace", sw.toString)
        coll.saveSnapshot("cdgp.error")
        e.printStackTrace()
    }
  }



  // --------------------------------------------------------------------------
  //                                 MAIN
  // --------------------------------------------------------------------------

  def main(args: Array[String]): Unit = {
    implicit val opt = getOptions(args)
    val useRegression = opt.paramBool("regression", false)
    if (useRegression)
      Regression.main(args)
    else
      run(opt)
  }

}

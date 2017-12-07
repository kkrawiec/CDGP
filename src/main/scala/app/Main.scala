package app

import java.io.{File, PrintWriter, StringWriter}

import fuel.func.RunExperiment
import fuel.util.{CollectorFile, Options, OptionsMap, Rng}
import swim.tree.Op
import cdgp._
import fuel.core.StatePop

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


/**
  * Entry point to run various pre-built variants of CDGP.
  *
  * Obligatory options:
  * --searchAlgorithm, accepted values: GP, GPSteadyState, Lexicase, LexicaseSteadyState
  * --benchmark, path to the SyGuS benchmark
  * --solverPath, path to the SMT solver (e.g. Z3)
  */
object Main {

  def runWithTimeout[T](timeoutMs: Long)(f: => T) : T = {
    Await.result(Future(f), Duration(timeoutMs, MILLISECONDS))
  }

  def getOptions(args: Array[String]): Options = {
    val opt = Options(args)
    if (opt.getOption("loadOptionsFromFile").isDefined) {
      println("Options loaded from file: " + opt.paramString("loadOptionsFromFile"))
      val tmp = Options.loadFromFile(new File(opt.getOption("loadOptionsFromFile").get))
      new OptionsMap(tmp.allOptions.filter{ case (k, v) => !k.startsWith("result") })
    }
    else opt
  }

  def main(args: Array[String]): Unit = {
    implicit val opt = getOptions(args)
    implicit val coll = CollectorFile(opt)
    implicit val rng = Rng(opt)

    def watchTimeFInt(alg: CDGPAlgorithm[Op, FInt], f: => Option[StatePop[(Op, FInt)]]): Option[StatePop[(Op, FInt)]] = {
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
            Common.epilogueEvalInt(alg.cdgpState, alg.bsf)(alg.pop.get)
          }
          coll.saveSnapshot("cdgp")
          alg.pop
      }
    }

    def watchTimeFSeqInt(alg: CDGPAlgorithm[Op, FSeqInt], f: => Option[StatePop[(Op, FSeqInt)]]): Option[StatePop[(Op, FSeqInt)]] = {
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
            Common.epilogueEvalSeqInt(alg.cdgpState, alg.bsf)(alg.pop.get)
          }
          coll.saveSnapshot("cdgp")
          alg.pop
      }
    }


    // --------------------------------------------------------------------------
    //                             MAIN LOOP
    // --------------------------------------------------------------------------
    try {
      val benchmark = opt('benchmark)
      println(s"Benchmark: $benchmark")
      val cdgpState = CDGPState(benchmark)

      val (res, bestOfRun) = opt('searchAlgorithm) match {
        case "GP" =>
          val alg = CDGPGenerational(cdgpState)
          val finalPop = watchTimeFInt(alg, RunExperiment(alg))
          (finalPop, alg.bsf.bestSoFar)

        case "GPSteadyState" =>
          val alg = CDGPSteadyState(cdgpState)
          val finalPop = watchTimeFInt(alg, RunExperiment(alg))
          (finalPop, alg.bsf.bestSoFar)

        case "Lexicase" =>
          val alg = CDGPGenerationalLexicase(cdgpState)
          val finalPop = watchTimeFSeqInt(alg, RunExperiment(alg))
          (finalPop, alg.bsf.bestSoFar)

        case "LexicaseSteadyState" =>
          val alg = CDGPSteadyStateLexicase(cdgpState)
          val finalPop = watchTimeFSeqInt(alg, RunExperiment(alg))
          (finalPop, alg.bsf.bestSoFar)
      }
      coll.saveSnapshot("cdgp")

      /////////////////////// Printing results ///////////////////////////////

      def isOptimal(bestOfRun: (Op, Any)): Boolean = {
        bestOfRun._2 match {
          case a if a.isInstanceOf[FInt] => a.asInstanceOf[FInt].correct
          case a if a.isInstanceOf[FSeqInt] => a.asInstanceOf[FSeqInt].correct
        }
      }


      assume(bestOfRun.isDefined, "No solution (optimal or approximate) to the problem was found.")
      val passedTestsRatio = coll.getResult("best.passedTestsRatio").getOrElse("n/a")
      val pn = 26
      println("\n")
      println("Best program found:".padTo(pn, ' ') + coll.getResult("bestOrig.smtlib").getOrElse("n/a"))
      println("Simplified:".padTo(pn, ' ') + coll.getResult("best.smtlib").getOrElse("n/a"))
      println("Evaluation:".padTo(pn, ' ') + coll.getResult("best.eval").getOrElse("n/a"))
      println("Program size:".padTo(pn, ' ') + coll.getResult("best.size").get)
      println("Ratio of passed tests:".padTo(pn, ' ') + passedTestsRatio)
      println("Tests total:".padTo(pn, ' ') + cdgpState.testsManager.getNumberOfTests)
      println("Tests known outputs:".padTo(pn, ' ') + cdgpState.testsManager.getNumberOfKnownOutputs)
      println("Total solver calls:".padTo(pn, ' ') + cdgpState.solver.getNumCalls)
      println("Generations:".padTo(pn, ' ') + coll.getResult("best.generation").get)
      println("Total time [s]:".padTo(pn, ' ') + coll.getResult("totalTimeSystem").get.toString.toDouble / 1000.0)
      println("Log file:".padTo(pn, ' ') + coll.get("thisFileName").get.toString)

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
}

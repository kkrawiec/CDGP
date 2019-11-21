package app

import java.io.File

import fuel.core.StatePop
import fuel.util._
import swim.tree.Op
import cdgp._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random


/**
  * Entry point to run various pre-built variants of CDGP.
  *
  * Obligatory options:
  * --benchmark, path to the SyGuS benchmark
  * --solverPath, path to the SMT solver (e.g. Z3)
  *
  * Main evolution options:
  * --selection, selection algorithm. Values: lexicase, tournament
  * --evolutionMode. Values: generational, steadyState
  */
object Main {

  def watchTimeMultipop[E <: Fitness](alg: CDGPAlgorithm[Op, E], f: => Option[Seq[StatePop[(Op, E)]]])
                                     (implicit coll: Collector, opt: Options): Option[StatePop[(Op, E)]] = {
    val maxTime = opt("multipop.maxTime", 86400000)  // 24h in miliseconds
    try {
      val finalPops = runWithTimeout(maxTime)(f)
      if (finalPops.isDefined) Option(StatePop(finalPops.get.flatten)) else None
    }
    catch {
      case e: java.util.concurrent.TimeoutException =>
        println("Timeout!!!!!!!!!!!!!!!!!!!")
        coll.set("cdgp.wasTimeout", true)
        coll.set("result.totalTimeSystem", maxTime)  // save in ms
        if (alg.pop.isDefined) {
          alg.bsf(alg.pop.get) // update bsf
          alg.reportStats(alg.pop.get)
        }
        alg.pop
    }
  }

  def runWithTimeout[T](timeoutMs: Long)(f: => T) : T = {
    Await.result(Future(f), Duration(timeoutMs, MILLISECONDS))
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
        coll.set("cdgp.wasTimeout", "true")
        coll.set("result.totalTimeSystem", maxTime)  // save in ms
        if (alg.pop.isDefined) {
          alg.bsf(alg.pop.get) // update bsf
          alg.reportStats(alg.pop.get)
        }
        else
          throw InitializationFailedException()

        // If there was a timeout, something could be already sent to the solver through the pipe,
        // and in order to avoid problems we need to clear the pipe.
        try {
          alg.cdgpState.solver.close()
        }
        finally {
          alg.cdgpState.solver.open()
        }

        coll.saveSnapshot("cdgp")
        alg.pop
    }
  }

  def loadReplayOptions(opt: Options): Options = {
    println("Options loaded from file: " + opt.paramString("optionsFile"))
    val tmp = Options.loadFromFile(new File(opt.getOption("optionsFile").get))
    new OptionsMap(tmp.allOptions.filter{ case (k, v) => !k.startsWith("result") })
  }

  def getOptions(args: Array[String]): Options = {
    val opt = Options(args)
    val opt2 = if (opt.getOption("optionsFile").isDefined) loadReplayOptions(opt) else opt
    CDGPOptions.validator.process(opt2)
  }

  def systemOptions(args: Array[String]): Boolean = {
    if (args.contains("--version")) { print("2018.1.0"); true }
    else if (args.contains("--help")) { CDGPOptions.validator.printOptions(); true }
    else false
  }



  // --------------------------------------------------------------------------
  //                                 MAIN
  // --------------------------------------------------------------------------

  def main(args: Array[String]): Unit = {
    if (systemOptions(args))
      sys.exit()
    val opt = getOptions(args ++ Array("--parEval", "false"))  // ensure that "--parEval false" is used
    Random.setSeed(opt('seed, 0))
    val useRegression = opt.paramBool("regression", false)
    if (useRegression)
      RegressionCDGP.run(opt)
    else
      CDGP.run(opt)
  }

}

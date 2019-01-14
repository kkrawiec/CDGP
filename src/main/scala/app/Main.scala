package app

import java.io.File
import fuel.core.StatePop
import fuel.util._
import swim.tree.Op
import cdgp._
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
  * --evolutionMode. Values: generational, steadyState
  */
object Main {

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
        println("Timeout!!!!!!!!!!!!!!!!!!!")
        coll.set("cdgp.wasTimeout", true)
        coll.set("result.totalTimeSystem", maxTime)  // save in ms
        if (alg.pop.isDefined) {
          alg.bsf(alg.pop.get) // update bsf
          alg.reportStats(alg.pop.get)
        }
        coll.saveSnapshot("cdgp")
        alg.pop
    }
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


  // --------------------------------------------------------------------------
  //                                 MAIN
  // --------------------------------------------------------------------------

  def main(args: Array[String]): Unit = {
    val opt = getOptions(args ++ Array("--parEval", "false")) // ensure that --parEval false is used
    val useRegression = opt.paramBool("regression", false)
    if (useRegression)
      RegressionCDGP.run(opt)
    else
      CDGP.run(opt)
  }

}

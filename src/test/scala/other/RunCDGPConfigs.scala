package tests.other

import java.io.File

import cdgp._
import fuel.func.RunExperiment
import fuel.util.{CollectorStdout, Options, Rng}
import tests.Global

object RunCDGPConfigs extends App {

  def runConfig(options: Options): Unit = {
    implicit val opt = options
    implicit val rng = Rng(Options(opt))
    implicit val coll = CollectorStdout(opt)
    val cdgpState = CDGPState(opt('benchmark))
    val selection = opt('selection)
    val evoMode = opt('evolutionMode)
    val (res, bestOfRun) = (selection, evoMode) match {
      case ("tournament", "generational") =>
        val alg = CDGPGenerational(cdgpState)
        val finalPop = RunExperiment(alg)
        (finalPop, alg.bsf.bestSoFar)

      case ("tournament", "steadyState") =>
        val alg = CDGPSteadyState(cdgpState)
        val finalPop = RunExperiment(alg)
        (finalPop, alg.bsf.bestSoFar)

      case ("lexicase", "generational") =>
        val alg = CDGPGenerationalLexicase(cdgpState)
        val finalPop = RunExperiment(alg)
        (finalPop, alg.bsf.bestSoFar)

      case ("lexicase", "steadyState") =>
        val alg = CDGPSteadyStateLexicase(cdgpState)
        val finalPop = RunExperiment(alg)
        (finalPop, alg.bsf.bestSoFar)
    }
  }


  val root = System.getProperty("user.dir")
  println(s"Working directory: $root")
  val collection = "/resources/LIA/cdgp_paper17"
  val files = Tools.getRecursiveListOfFiles(new File(root + collection)).
    filter{ f =>f.getName.endsWith(".sl") }

  for (file <- files) {
    println("-" * 100)
    println(s"File: ${file.getAbsolutePath}")
    println("-" * 100)
    for (method <- Seq("CDGP", "GPR"))
      for (sel <- Seq("generational", "lexicase"))
        for (evoMode <- Seq("generational", "steadyState")) {
          val options = Options(Global.solverConfig + s" --benchmark ${file.getAbsolutePath}" +
            s" --method $method --selection $sel --evolutionMode $evoMode --populationSize 50 --maxGenerations 10 --seed 0")
          runConfig(options)
      }
  }
}

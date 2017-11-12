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
    val (res, bestOfRun) = opt('searchAlgorithm) match {
      case "GP" =>
        val alg = CDGPGenerational(cdgpState)
        val finalPop = RunExperiment(alg)
        (finalPop, alg.bsf.bestSoFar)

      case "GPSteadyState" =>
        val alg = CDGPSteadyState(cdgpState)
        val finalPop = RunExperiment(alg)
        (finalPop, alg.bsf.bestSoFar)

      case "Lexicase" =>
        val alg = CDGPGenerationalLexicase(cdgpState)
        val finalPop = RunExperiment(alg)
        (finalPop, alg.bsf.bestSoFar)

      case "LexicaseSteadyState" =>
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
      for (searchAlg <- Seq("GP", "GPSteadyState", "Lexicase", "LexicaseSteadyState")) {
        val options = Options(s"--solverPath ${Global.solverPath} --benchmark ${file.getAbsolutePath}" +
          s" --method $method --searchAlgorithm $searchAlg --populationSize 50 --maxGenerations 10 --seed 0")
        runConfig(options)
      }
  }
}

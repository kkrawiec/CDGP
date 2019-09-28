package misc

import java.io.File

import app.{OptionInfo, OptionsValidator}
import cdgp.{LoadSygusBenchmark, SygusProblemData, TemplateVerification, Tools}
import fuel.util.{Options, OptionsMap}

import scala.collection.mutable
import misc.benchmarks.BenchmarksGECCO19

object SolutionVerifier extends App {
  def loadOptions(path: String): Options = loadOptions(new File(path))
  def loadOptions(file: File): Options = {
    println("Options loaded from file: " + file.getAbsolutePath)
    Options.loadFromFile(file)
  }

  def verifySolution(opt: Options, bpathRoot0: String = ""): (Boolean, Option[String]) = {
    val bpathRoot = if (bpathRoot0.last=="/") bpathRoot0 else bpathRoot0 + "/"
    val bpath = bpathRoot + opt('benchmark)
    val sygusData = SygusProblemData(LoadSygusBenchmark(bpath))
    val templateVer = new TemplateVerification(sygusData)
    ???
  }

  def processFile(options: Options): Unit = {
    if (options.paramString("method") == "GP") {
      println(s"Processing file: $options")
    }
  }

  val args2 = mutable.MutableList[OptionInfo]()
  args2 += OptionInfo("dir", "String", required=true, desc="Path to a folder with property files.")

  val RB = BenchmarksGECCO19
  val constr_gravity = RB.b_gravity_b



  val dirpath = "exp_results"

  // Recursively traverse directory and collect property files
  val files = Tools.getRecursiveListOfFiles(new File(dirpath)).filter{ f => f.getName.endsWith(".sl")}


  // For every file, verify the solution for different collections of constraints for the given benchmark
  files.foreach(f => processFile(loadOptions(f)))
}

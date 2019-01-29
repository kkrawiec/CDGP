package misc

import java.io.File

import app.{OptionInfo, OptionsValidator}
import cdgp.Tools
import fuel.util.{Options, OptionsMap}

import scala.collection.mutable

object SolutionVerifier extends App {
  def loadOptions(path: String): Options = loadOptions(new File(path))
  def loadOptions(file: File): Options = {
    println("Options loaded from file: " + file.getAbsolutePath)
    Options.loadFromFile(file)
  }

  def processFile(options: Options): Unit = {
    if (options.paramString("method") == "GP") {
      println(s"Processing file: $options")
    }
  }

  val args2 = mutable.MutableList[OptionInfo]()
  args2 += OptionInfo("dir", "String", required=true, desc="Path to a folder with property files.")

  val RB = RegressionBenchmarks
  val constr_gravity = RB.b_gravity_b



  val dirpath = "exp_results"

  // Recursively traverse directory and collect property files
  val files = Tools.getRecursiveListOfFiles(new File(dirpath)).filter{ f => f.getName.endsWith(".sl")}


  // For every file, verify the solution for different collections of constraints for the given benchmark
  files.foreach(f => processFile(loadOptions(f)))
}

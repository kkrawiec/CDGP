package misc.benchmarks
//package main.scala.misc.benchmarks

import Utils._
import fuel.util.{Options, Rng}
import misc._

import java.io.File


/**
  * Real-world benchmarks used in the paper for TEVC.
  */
object BenchmarksTEVC extends App {

  def loadCsvBenchmark(path: String, sep: String = ";"): Seq[Seq[String]] = {
    val bufferedSource = io.Source.fromFile(path)
    val lines: Seq[String] = bufferedSource.getLines.toList
    bufferedSource.close
    val result = lines.map{line =>
      line.split(sep).map(_.trim).toSeq
    }.toSeq
    result
  }

  // The last column of ComputerHardware is estimation by linear regression, so we remove this data, as well as any text attributes
  val data_computerHardware:  Seq[Seq[Double]] = loadCsvBenchmark("resources/NRA/real_world/ComputerHardware/ComputerHardware.csv").map(_.slice(2,9).map(_.toDouble))
  println(data_computerHardware.mkString("\n"))
  val varNames = 1.to(data_computerHardware.head.size-1).map(x => f"x$x")
  val b_computerHardware = Benchmark("ComputerHardware", varNames,
    Seq(
      PropOutputBound(Some(0.0), None, range=rangesGeqZero("x1", "x2", "x3", "x4", "x5", "x6")),
      PropDescending("x1", range=rangesGeqZero("x1", "x2", "x3", "x4", "x5", "x6"), strict=false),
//      PropAscending("x2", range=rangesGeqZero("x1", "x2", "x3", "x4", "x5", "x6"), strict=false),
//      PropAscending("x3", range=rangesGeqZero("x1", "x2", "x3", "x4", "x5", "x6"), strict=false),
      PropAscending("x4", range=rangesGeqZero("x1", "x2", "x3", "x4", "x5", "x6"), strict=false)
//      PropAscending("x5", range=rangesGeqZero("x1", "x2", "x3", "x4", "x5", "x6"), strict=false),
//      PropAscending("x6", range=rangesGeqZero("x1", "x2", "x3", "x4", "x5", "x6"), strict=false)
    ),
    data_computerHardware.map(row => (row.take(row.size-1), row.last)))
  // MANUALLY ADD TO THE .sl FILE: (precondition (and (>= x1 0.0) (>= x2 0.0) (>= x3 0.0) (>= x4 0.0) (>= x5 0.0) (>= x6 0.0)))

  def generateSygusFile(b: Benchmark, benchmarkData: Seq[Seq[Double]], dirPath: String): Unit = {
    ensureDir(dirPath)
    ensureDir(dirPath + "/tsv")

    val sygusCode = RegressionConstraints.generateInSygusFormat(b, logic="NRA")
    val tsvCode = RegressionConstraints.generateInCsvFormat(b, delimiter="\t", useFunctionNameAsTarget=false)
    val pathSl = s"${dirPath}/${b.fileName(extension=".sl")}"
    val pathTsv = s"${dirPath}/tsv/${b.fileName(extension=".tsv")}"
    println(sygusCode)
    println("\n\n")
    println(tsvCode)
    Utils.saveFile(pathSl, sygusCode)
    Utils.saveFile(pathTsv, tsvCode)
  }

  generateSygusFile(b_computerHardware, data_computerHardware, "resources/NRA/tevc")
}

package misc.benchmarks

import java.io.File
import Utils._
import misc.{Benchmark, PropAscending, RegressionConstraints}


object BenchmarksGPEM extends App {
  def fGravity(vars: Seq[Double]): Double = (6.674e-11 * vars(0) * vars(1)) / (vars(2) * vars(2))
  def fKeijzer12(vars: Seq[Double]): Double = vars(0) * vars(0) * vars(0) * vars(0) - vars(0) * vars(0) * vars(0) + 0.5 * vars(1) * vars(1) - vars(1)

  val b_gravity_m = Benchmark("gravity_m", Seq("m1", "m2", "r"),
    Seq(
      PropAscending("m1", range=rangesGtZero("m1", "m2", "r"), strict=true),
      PropAscending("m2", range=rangesGtZero("m1", "m2", "r"), strict=true)
    ))



  val ns = Seq(5, 100)

  val benchmarks = Seq(
    ns.map{ n => Benchmark(b_gravity_m, generateTestsU(3, n, fGravity, 0.0001, 20.0)) }
  ).flatten



  //////////////////////////////////////////////////////////////////////////

  val dirPath = "resources/NRA/gpem"

  val dir = new File(dirPath)
  if (dir.exists())
    dir.delete()
  dir.mkdir()

  benchmarks.foreach{ b =>
    val sygusCode = RegressionConstraints.generateSygusCode(b, logic="NRA")
    val path = s"${dirPath}/${b.fileName}"
    println(sygusCode)
    println("\n\n")
    Utils.saveFile(path, sygusCode)
  }
}

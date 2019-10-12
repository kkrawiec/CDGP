package misc.benchmarks

import java.io.File
import Utils._
import misc.{Benchmark, PropAscending, RegressionConstraints}


object BenchmarksGPEM extends App {
  def fGravity(vars: Seq[Double]): Double = (6.674e-11 * vars(0) * vars(1)) / (vars(2) * vars(2))
  def fNguyen1(vars: Seq[Double]): Double = Math.pow(vars(0), 3) + Math.pow(vars(0), 2) + vars(0)
  def fNguyen3(vars: Seq[Double]): Double = Math.pow(vars(0), 5) + Math.pow(vars(0), 4) + Math.pow(vars(0), 3) + Math.pow(vars(0), 2) + vars(0)
  def fNguyen4(vars: Seq[Double]): Double = Math.pow(vars(0), 6) + Math.pow(vars(0), 5) + Math.pow(vars(0), 4) + Math.pow(vars(0), 3) + Math.pow(vars(0), 2) + vars(0)
  def fKeijzer12(vars: Seq[Double]): Double = Math.pow(vars(0), 4) - Math.pow(vars(0), 3) + 0.5 * Math.pow(vars(1), 2) - vars(1)
  def fKeijzer14(vars: Seq[Double]): Double = 8.0 / (2.0 + Math.pow(vars(0), 2) + Math.pow(vars(1), 2))
  def fKeijzer15(vars: Seq[Double]): Double = Math.pow(vars(0), 3) / 5.0 + Math.pow(vars(1), 3) / 2.0 - vars(0) - vars(1)
  def fPagie1(vars: Seq[Double]): Double = 1.0 / (1.0 + Math.pow(vars(0), -4)) + 1.0 / (1.0 + Math.pow(vars(1), -4))

  val b_gravity_m = Benchmark("gravity_m", Seq("m1", "m2", "r"),
    Seq(
      PropAscending("m1", range=rangesGtZero("m1", "m2", "r"), strict=true),
      PropAscending("m2", range=rangesGtZero("m1", "m2", "r"), strict=true)
    ))
  val b_keijzer12 = Benchmark("keijzer12", Seq("x", "y"),
    Seq( //TODO: properties
      PropAscending("m1", range=rangesGtZero("x", "y"), strict=true),
      PropAscending("m2", range=rangesGtZero("x", "y"), strict=true)
    ))
  val b_keijzer14 = Benchmark("keijzer14", Seq("x", "y"),
    Seq( //TODO: properties
      PropAscending("m1", range=rangesGtZero("x", "y"), strict=true),
      PropAscending("m2", range=rangesGtZero("x", "y"), strict=true)
    ))
  val b_keijzer15 = Benchmark("keijzer15", Seq("x", "y"),
    Seq( //TODO: properties
      PropAscending("m1", range=rangesGtZero("x", "y"), strict=true),
      PropAscending("m2", range=rangesGtZero("x", "y"), strict=true)
    ))
  val b_pagie1 = Benchmark("pagie1", Seq("x", "y"),
    Seq( //TODO: properties
      PropAscending("m1", range=rangesGtZero("x", "y"), strict=true),
      PropAscending("m2", range=rangesGtZero("x", "y"), strict=true)
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

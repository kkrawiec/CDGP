package misc.benchmarks

import java.io.File
import Utils._
import misc._


object BenchmarksGPEM extends App {
  def fGravity(vars: Seq[Double]): Double = (6.674e-11 * vars(0) * vars(1)) / (vars(2) * vars(2))
  def fResistancePar2(vars: Seq[Double]): Double = (vars(0) * vars(1)) / (vars(0) + vars(1))
  def fResistancePar3(vars: Seq[Double]): Double = (vars(0) * vars(1) * vars(2)) / (vars(0)*vars(1) + vars(0)*vars(2) + vars(1)*vars(2))
  // The nguyen benchmarks specified below are the only ones supported by SMT solvers
  def fNguyen1(vars: Seq[Double]): Double = Math.pow(vars(0), 3) + Math.pow(vars(0), 2) + vars(0)
  def fNguyen3(vars: Seq[Double]): Double = Math.pow(vars(0), 5) + Math.pow(vars(0), 4) + Math.pow(vars(0), 3) + Math.pow(vars(0), 2) + vars(0)
  def fNguyen4(vars: Seq[Double]): Double = Math.pow(vars(0), 6) + Math.pow(vars(0), 5) + Math.pow(vars(0), 4) + Math.pow(vars(0), 3) + Math.pow(vars(0), 2) + vars(0)
  // The keijzer benchmarks specified below are the only ones supported by SMT solvers
  def fKeijzer5(vars: Seq[Double]): Double = 30.0 * vars(0) * vars(2) / ((vars(0) - 10.0) * vars(1)*vars(1))
  def fKeijzer12(vars: Seq[Double]): Double = Math.pow(vars(0), 4) - Math.pow(vars(0), 3) + 0.5 * Math.pow(vars(1), 2) - vars(1)
  def fKeijzer14(vars: Seq[Double]): Double = 8.0 / (2.0 + Math.pow(vars(0), 2) + Math.pow(vars(1), 2))
  def fKeijzer15(vars: Seq[Double]): Double = Math.pow(vars(0), 3) / 5.0 + Math.pow(vars(1), 3) / 2.0 - vars(0) - vars(1)
  def fPagie1(vars: Seq[Double]): Double = 1.0 / (1.0 + Math.pow(vars(0), -4)) + 1.0 / (1.0 + Math.pow(vars(1), -4))


  // --------------------------------------------------------------------------------------
  // Benchmarks from GECCO 2019

  // Task: calculate the force of gravity between two bodies
  // Solution: (6.674e-11 * m1 * m2) / r^2
  val b_gravity = Benchmark("gravity", Seq("m1", "m2", "r"),
    Seq(
      PropVarSymmetry2("m1", "m2", rangesGtZero("m1", "m2", "r")),
      PropOutputBound(Some(0.0), None, range=rangesGtZero("m1", "m2", "r")),
      PropAscending("m1", range=rangesGtZero("m1", "m2", "r"), strict=true),
      PropAscending("m2", range=rangesGtZero("m1", "m2", "r"), strict=true)
    ))
  // Task: calculate the total resistance of 2 parallel resistors
  // Solution: (r1 * r2) / (r1 + r2)
  val b_resistance_par2 = Benchmark("resistance_par2", Seq("r1", "r2"),
    Seq(
      PropVarSymmetry2("r1", "r2", rangesGtZero("r1", "r2")),
      CustomConstraint("(=> (= r1 r2) (= {0} (/ r1 2.0)))", range=rangesGtZero("r1", "r2")),
      CustomConstraint("(and (<= {0} r1) (<= {0} r2))", range=rangesGtZero("r1", "r2"))
    ))
  // Task: calculate the total resistance of 3 parallel resistors
  // Solution: (r1 * r2 * r3) / (r1*r2 + r2*r3 + r2*r3)
  val b_resistance_par3 = Benchmark("resistance_par3", Seq("r1", "r2", "r3"),
    Seq(
      PropVarSymmetry2("r1", "r2", rangesGtZero("r1", "r2", "r3")),
      PropVarSymmetry2("r1", "r3", rangesGtZero("r1", "r2", "r3")),
      PropVarSymmetry2("r2", "r3", rangesGtZero("r1", "r2", "r3")),
      CustomConstraint("(=> (= r1 r2 r3) (= {0} (/ r1 3.0)))", range=rangesGtZero("r1", "r2", "r3")),
      CustomConstraint("(and (<= {0} r1) (<= {0} r2) (<= {0} r3))", range=rangesGtZero("r1", "r2", "r3"))
    ))

  // --------------------------------------------------------------------------------------

  val b_keijzer5 = Benchmark("keijzer5", Seq("x", "y"),
    Seq( //TODO: properties
    ))
  val b_keijzer12 = Benchmark("keijzer12", Seq("x", "y"),
    Seq(
      CustomConstraint("(=> (> y 1.0)  (<= (keijzer12 x y) (keijzer12 (- x) y))  )", range=rangesGtZero("x"))
    ))
  val b_keijzer14 = Benchmark("keijzer14", Seq("x", "y"),
    Seq( //TODO: properties
    ))
  val b_keijzer15 = Benchmark("keijzer15", Seq("x", "y"),
    Seq( //TODO: properties
    ))
  val b_nguyen1 = Benchmark("nguyen1", Seq("x"),
    Seq(
      PropOutputBound(Some(0.0), None, range=rangesGtZero("x")),
      PropOutputBound(None, Some(0.0), range=rangesLtZero("x")),
      CustomConstraint("(>= (nguyen1 x) (nguyen1 (- x)))", range=rangesGtZero("x"))
    ))
  val b_nguyen3 = Benchmark("nguyen3", Seq("x"),
    Seq(
      PropOutputBound(Some(0.0), None, range=rangesGtZero("x")),
      PropOutputBound(None, Some(0.0), range=rangesLtZero("x")),
      CustomConstraint("(>= (nguyen3 x) (nguyen3 (- x)))", range=rangesGtZero("x"))
    ))
  val b_nguyen4 = Benchmark("nguyen4", Seq("x"),
    Seq(
      PropOutputBound(Some(0.0), None, range=rangesGtZero("x")),
      PropOutputBound(None, Some(-0.75), range=rangesLtZero("x")),
      CustomConstraint("(>= (nguyen4 x) (nguyen4 (- x)))", range=rangesGtZero("x"))
    ))



  val ns = Seq(500)  // the number of data; will be later divided in CDGP on training and test set

  val benchmarks = Seq(
    ns.map{ n => Benchmark(b_gravity, generateTestsU(3, n, fGravity, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par2, generateTestsU(2, n, fResistancePar2, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par3, generateTestsU(3, n, fResistancePar3, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_nguyen1, generateTestsU(1, n, fNguyen1, -10.0, 10.0)) },
    ns.map{ n => Benchmark(b_nguyen3, generateTestsU(1, n, fNguyen3, -10.0, 10.0)) },
    ns.map{ n => Benchmark(b_nguyen4, generateTestsU(1, n, fNguyen4, -10.0, 10.0)) }
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

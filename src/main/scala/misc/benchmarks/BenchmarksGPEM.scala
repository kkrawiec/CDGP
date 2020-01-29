package misc.benchmarks

import java.io.File

import Utils._
import fuel.util.{Options, Rng}
import misc._


object BenchmarksGPEM extends App {
  def fIdentity(vars: Seq[Double]): Double = vars(0)
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


  val b_identity = Benchmark("identity", Seq("x"), Seq())

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

  val b_keijzer5 = Benchmark("keijzer5", Seq("x", "y", "z"),
    Seq(
      CustomConstraint("(=> (or (= x 0.0) (= z 0.0)) (= (keijzer5 x y z) 0.0) )", range=Seq(Range.diffThan("y", 0.0), Range.diffThan("x", 10.0))),
      CustomConstraint("(=> (and (= x y) (= y z) (> x 0.0)) (> (keijzer5 x y z) 10.0) )", range=Seq(Range.diffThan("y", 0.0), Range.diffThan("x", 10.0))),
      CustomConstraint("(=> (and (= x y) (= y z) (< x 0.0)) (< (keijzer5 x y z) 10.0) )", range=Seq(Range.diffThan("y", 0.0), Range.diffThan("x", 10.0)))
    ))
  val b_keijzer12 = Benchmark("keijzer12", Seq("x", "y"),
    Seq(
      CustomConstraint("(=> (>= x 0.0)  (<= (keijzer12 x y) (keijzer12 (- x) y))  )"),
      CustomConstraint("(=> (>= y 0.0)  (<= (keijzer12 x y) (keijzer12 x (- y)))  )"),
      CustomConstraint("(=> (and (= x 0.0) (= y 0.0)) (= (keijzer12 x y) 0.0) )"),
      //PropAscending("x", range=Seq(Range("x", Some(0.0), lbSign=">=")), strict=true), // After testing in the solver I determined that it is false
      PropDescending("x", range=Seq(Range("x", None, Some(0.0), ubSign="<=")), strict=true),
      PropAscending("y", range=Seq(Range("y", Some(1.0), lbSign=">=")), strict=true),
      PropDescending("y", range=Seq(Range("y", None, Some(1.0), ubSign="<=")), strict=true)
    ))
  val b_keijzer14 = Benchmark("keijzer14", Seq("x", "y"),
    Seq(
      PropOutputBound(Some(0.0), None, lbSign=">="),
      PropOutputBound(None, Some(4.0), ubSign="<="),
      CustomConstraint("(<= (keijzer14 x y) (keijzer14 0.0 0.0))"),
      PropVarSymmetry2("x", "y")
    ))
  val b_keijzer15 = Benchmark("keijzer15", Seq("x", "y"),
    Seq(
      CustomConstraint("(=> (and (= x 0.0) (= y 0.0)) (= (keijzer15 x y) 0.0) )"),
      CustomConstraint("(=> (and (<= x 0.0) (= x (- y))) (>= (keijzer15 x y) 0.0) )"),
      CustomConstraint("(=> (and (>= x 0.0) (= x (- y))) (<= (keijzer15 x y) 0.0) )")
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
  val b_pagie1 = Benchmark("pagie1", Seq("x", "y"),
    Seq(
      PropOutputBound(Some(0.0), None),
      PropOutputBound(None, Some(2.0)),
      PropVarSymmetry2("x", "y")
    ))



  val ns = Seq(500)  // the number of data; will be later divided in CDGP on training and test set

  val benchmarks = Seq(
//    ns.map{ n => Benchmark(b_identity, generateTestsU(1, n, fIdentity, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_gravity, generateTestsU(3, n, fGravity, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par2, generateTestsU(2, n, fResistancePar2, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par3, generateTestsU(3, n, fResistancePar3, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_nguyen1, generateTestsU(1, n, fNguyen1, -10.0, 10.0)) },
    ns.map{ n => Benchmark(b_nguyen3, generateTestsU(1, n, fNguyen3, -10.0, 10.0)) },
    ns.map{ n => Benchmark(b_nguyen4, generateTestsU(1, n, fNguyen4, -10.0, 10.0)) },
    ns.map{ n => Benchmark(b_keijzer5, generateTestsU(3, n, fKeijzer5, -10.0, 10.0)) },
    ns.map{ n => Benchmark(b_keijzer12, generateTestsU(2, n, fKeijzer12, -10.0, 10.0)) },
    ns.map{ n => Benchmark(b_keijzer14, generateTestsU(2, n, fKeijzer14, -10.0, 10.0)) },
    ns.map{ n => Benchmark(b_keijzer15, generateTestsU(2, n, fKeijzer15, -10.0, 10.0)) },
    ns.map{ n => Benchmark(b_pagie1, generateTestsU(2, n, fPagie1, -10.0, 10.0)) }
  ).flatten



  //////////////////////////////////////////////////////////////////////////
  def generateNoNoise(benchmarks: Seq[Benchmark], dirPath: String): Unit = {
    ensureDir(dirPath)
    ensureDir(dirPath + "/tsv")

    benchmarks.foreach{ b =>
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
  }

  def generateWithNoise(benchmarks: Seq[Benchmark], dirPath: String, percentY: Double, percentX: Double = 0.0): Unit = {
    ensureDir(dirPath)
    ensureDir(dirPath + "/tsv")

    implicit val rng = Rng(Options("--seed 0"))
    val benchmarks2 = benchmarks.map(NoiseAdder.noiseNormalPercentB(_, percentY, percentX))

    benchmarks2.foreach{ b =>
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
  }

  // NOTE: the same set of generated benchmarks are reused for noise
  ensureDir("resources/NRA/gpem")
  generateNoNoise(benchmarks, "resources/NRA/gpem/noNoise")
  generateWithNoise(benchmarks, "resources/NRA/gpem/withNoise", 0.01, 0.01)
}

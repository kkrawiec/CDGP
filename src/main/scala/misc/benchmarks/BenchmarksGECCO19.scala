package misc.benchmarks

import java.io.File
import Utils._
import misc.{Benchmark, CustomConstraint, PropApproxDerivative, PropAscendingFreeVars, PropDescendingFreeVars, PropOutputBound, PropSymmetryYAxis, PropVarSymmetry2, PropVarSymmetry3, Range, RegressionConstraints}


object BenchmarksGECCO19 extends App {
  def fKeijzer12(vars: Seq[Double]): Double = vars(0) * vars(0) * vars(0) * vars(0) - vars(0) * vars(0) * vars(0) + 0.5 * vars(1) * vars(1) - vars(1)
  def fKoza1(vars: Seq[Double]): Double = vars(0) * vars(0) * vars(0) * vars(0) + vars(0) * vars(0) * vars(0) + vars(0) * vars(0) + vars(0)
  def fSquares1(vars: Seq[Double]): Double = vars(0) * vars(0)
  def fSquares2(vars: Seq[Double]): Double = vars(0) * vars(0) + vars(1) * vars(1)
  def fSquares3(vars: Seq[Double]): Double = vars(0) * vars(0) + vars(1) * vars(1) + vars(2) * vars(2)
  def fGravity(vars: Seq[Double]): Double = (6.674e-11 * vars(0) * vars(1)) / (vars(2) * vars(2))
  def fGravityNoG(vars: Seq[Double]): Double = (vars(0) * vars(1)) / (vars(2) * vars(2))
  def fResistancePar2(vars: Seq[Double]): Double = (vars(0) * vars(1)) / (vars(0) + vars(1))
  def fResistancePar3(vars: Seq[Double]): Double = (vars(0) * vars(1) * vars(2)) / (vars(0)*vars(1) + vars(0)*vars(2) + vars(1)*vars(2))


  val b_squares1 = Benchmark("squares1", Seq("x"), // x^2
    Seq(
      PropApproxDerivative("x", 1.0, 2.0, degree=1),
      PropSymmetryYAxis("x"),
      PropOutputBound(Some(0.0), None),
      PropAscendingFreeVars("x", range=rangesGtZero("x")),
      PropDescendingFreeVars("x", range=rangesLtZero("x"))
    ))

  val b_squares2 = Benchmark("squares2", Seq("x", "y"), // x^2 + y^2
    Seq(
      PropApproxDerivative("x", 1.0, 2.0, degree=1),
      PropApproxDerivative("x", 1.0, 2.0, degree=2),
      PropApproxDerivative("x", 1.0, 0.0, degree=3),
      PropSymmetryYAxis("x"),
      PropOutputBound(Some(0.0), None),
      PropAscendingFreeVars("x", range=rangesGtZero("x")),
      PropDescendingFreeVars("x", range=rangesLtZero("x"))
    ))
  val b_squares2_tests = Benchmark("squares2_tests", Seq("x", "y"), Seq())
  val b_squares2_b = Benchmark("squares2_b", Seq("x", "y"),
    Seq(PropOutputBound(Some(0.0), None)))
  val b_squares2_s = Benchmark("squares2_s", Seq("x", "y"),
    Seq(PropVarSymmetry2("x", "y")))
  val b_squares2_bs = Benchmark("squares2_bs", Seq("x", "y"),
    Seq(PropOutputBound(Some(0.0), None), PropVarSymmetry2("x", "y")))

  val b_squares3_tests = Benchmark("squares3_tests", Seq("x", "y", "z"), Seq())
  val b_squares3_b = Benchmark("squares3_b", Seq("x", "y", "z"),
    Seq(PropOutputBound(Some(0.0), None)))
  val b_squares3_s = Benchmark("squares3_s", Seq("x", "y", "z"),
    Seq(PropVarSymmetry3("x", "y", "z")))
  val b_squares3_bs = Benchmark("squares3_bs", Seq("x", "y", "z"),
    Seq(PropOutputBound(Some(0.0), None), PropVarSymmetry3("x", "y", "z")))

  val b_keijzer12 = Benchmark("keijzer12", Seq("x", "y"), //x^4 - x^3 + y^2/2 - y
    Seq(
      PropApproxDerivative("x", 1.0, 1.0, degree=1),
      PropApproxDerivative("y", 1.0, 0.0, degree=1),
      PropOutputBound(Some(0.0), None, range=Seq(Range("x", Some(1.0), None), Range("y", 0.0, 0.0))),
      PropOutputBound(Some(0.0), None, range=Seq(Range("y", Some(2.0), None), Range("x", 0.0, 0.0))),
      PropAscendingFreeVars("x", range=Seq(Range("x", Some(1.0), None), Range("y", 0.0, 0.0))),
      PropAscendingFreeVars("y", range=Seq(Range("y", Some(2.0), None), Range("x", 0.0, 0.0)))
    ))
  val b_koza1 = Benchmark("koza1", Seq("x"), //x^4 + x^3 + x^2 + x
    Seq(
      PropApproxDerivative("x", 1.0, 10.0, degree=1),
      PropOutputBound(Some(-0.5), None),
      PropAscendingFreeVars("x", range=Seq(Range("x", Some(-0.25), None))),
      PropDescendingFreeVars("x", range=Seq(Range("x", None, Some(-0.75))))
    ))

  // Task: calculate the force of gravity between two bodies
  // Solution: (6.674e-11 * m1 * m2) / r^2
  val b_gravity = Benchmark("gravity", Seq("m1", "m2", "r"),
    Seq(
      PropVarSymmetry2("m1", "m2", rangesGtZero("m1", "m2", "r")),
      PropOutputBound(Some(0.0), None, range=rangesGtZero("m1", "m2", "r")),
      PropAscendingFreeVars("m1", range=rangesGtZero("m1", "m2", "r"), strict=true),
      PropAscendingFreeVars("m2", range=rangesGtZero("m1", "m2", "r"), strict=true)
    ))
  val b_gravity_s = Benchmark("gravity_s", Seq("m1", "m2", "r"),
    Seq(
      PropVarSymmetry2("m1", "m2", rangesGtZero("m1", "m2", "r"))
    ))
  val b_gravity_b = Benchmark("gravity_b", Seq("m1", "m2", "r"),
    Seq(
      PropOutputBound(Some(0.0), None, range=rangesGtZero("m1", "m2", "r"))
    ))
  val b_gravity_m = Benchmark("gravity_m", Seq("m1", "m2", "r"),
    Seq(
      PropAscendingFreeVars("m1", range=rangesGtZero("m1", "m2", "r"), strict=true),
      PropAscendingFreeVars("m2", range=rangesGtZero("m1", "m2", "r"), strict=true)
    ))
  val b_gravity_bm = Benchmark("gravity_bm", Seq("m1", "m2", "r"),
    Seq(
      PropOutputBound(Some(0.0), None, range=rangesGtZero("m1", "m2", "r")),
      PropAscendingFreeVars("m1", range=rangesGtZero("m1", "m2", "r"), strict=true),
      PropAscendingFreeVars("m2", range=rangesGtZero("m1", "m2", "r"), strict=true)
    ))
  val b_gravity_bs = Benchmark("gravity_bs", Seq("m1", "m2", "r"),
    Seq(
      PropVarSymmetry2("m1", "m2", rangesGtZero("m1", "m2", "r")),
      PropOutputBound(Some(0.0), None, range=rangesGtZero("m1", "m2", "r"))
    ))
  val b_gravity_ms = Benchmark("gravity_ms", Seq("m1", "m2", "r"),
    Seq(
      PropVarSymmetry2("m1", "m2", rangesGtZero("m1", "m2", "r")),
      PropAscendingFreeVars("m1", range=rangesGtZero("m1", "m2", "r"), strict=true),
      PropAscendingFreeVars("m2", range=rangesGtZero("m1", "m2", "r"), strict=true)
    ))
  val b_gravity_bms = Benchmark("gravity_bms", Seq("m1", "m2", "r"),
    Seq(
      PropVarSymmetry2("m1", "m2", rangesGtZero("m1", "m2", "r")),
      PropOutputBound(Some(0.0), None, range=rangesGtZero("m1", "m2", "r")),
      PropAscendingFreeVars("m1", range=rangesGtZero("m1", "m2", "r"), strict=true),
      PropAscendingFreeVars("m2", range=rangesGtZero("m1", "m2", "r"), strict=true)
    ))

  // Task: calculate the force of gravity between two bodies (not taking into account a constant)
  // Solution: (m1 * m2) / r^2
  val b_gravityNoG = Benchmark("gravity_noG", Seq("m1", "m2", "r"),
    Seq(
      PropVarSymmetry2("m1", "m2", range=rangesGtZero("m1", "m2", "r")),
      PropOutputBound(Some(0.0), None, range=rangesGtZero("m1", "m2", "r")),
      PropAscendingFreeVars("m1", range=rangesGtZero("m1", "m2", "r"), strict=true),
      PropAscendingFreeVars("m2", range=rangesGtZero("m1", "m2", "r"), strict=true)
    ))

  // Task: calculate the total resistance of 2 parallel resistors
  // Solution: (r1 * r2) / (r1 + r2)
  val b_resistance_par2 = Benchmark("resistance_par2", Seq("r1", "r2"),
    Seq(
      PropVarSymmetry2("r1", "r2", rangesGtZero("r1", "r2")),
      CustomConstraint("(=> (= r1 r2) (= {0} (/ r1 2.0)))", range=rangesGtZero("r1", "r2")),
      CustomConstraint("(and (<= {0} r1) (<= {0} r2))", range=rangesGtZero("r1", "r2"))
    ))
  val b_resistance_par2_s = Benchmark("resistance_par2_s", Seq("r1", "r2"),
    Seq(
      PropVarSymmetry2("r1", "r2", rangesGtZero("r1", "r2"))
    ))
  val b_resistance_par2_c1 = Benchmark("resistance_par2_c1", Seq("r1", "r2"),
    Seq(
      CustomConstraint("(=> (= r1 r2) (= {0} (/ r1 2.0)))", range=rangesGtZero("r1", "r2"))
    ))
  val b_resistance_par2_c2 = Benchmark("resistance_par2_c2", Seq("r1", "r2"),
    Seq(
      CustomConstraint("(and (<= {0} r1) (<= {0} r2))", range=rangesGtZero("r1", "r2"))
    ))
  val b_resistance_par2_sc = Benchmark("resistance_par2_sc", Seq("r1", "r2"),
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
  val b_resistance_par3_s = Benchmark("resistance_par3_s", Seq("r1", "r2", "r3"),
    Seq(
      PropVarSymmetry3("r1", "r2", "r3", rangesGtZero("r1", "r2", "r3"))
    ))
  val b_resistance_par3_c1 = Benchmark("resistance_par3_c1", Seq("r1", "r2", "r3"),
    Seq(
      CustomConstraint("(=> (= r1 r2 r3) (= {0} (/ r1 3.0)))", range=rangesGtZero("r1", "r2", "r3"))
    ))
  val b_resistance_par3_c2 = Benchmark("resistance_par3_c2", Seq("r1", "r2", "r3"),
    Seq(
      CustomConstraint("(and (<= {0} r1) (<= {0} r2) (<= {0} r3))", range=rangesGtZero("r1", "r2", "r3"))
    ))
  val b_resistance_par3_sc = Benchmark("resistance_par3_sc", Seq("r1", "r2", "r3"),
    Seq(
      PropVarSymmetry3("r1", "r2", "r3", rangesGtZero("r1", "r2", "r3")),
      CustomConstraint("(=> (= r1 r2 r3) (= {0} (/ r1 3.0)))", range=rangesGtZero("r1", "r2", "r3")),
      CustomConstraint("(and (<= {0} r1) (<= {0} r2) (<= {0} r3))", range=rangesGtZero("r1", "r2", "r3"))
    ))



  val ns = Seq(3, 5, 10)

  val benchmarks = Seq(
    ns.map{ n => Benchmark(b_keijzer12, generateTestsU(2, n, fKeijzer12, -20.0, 20.0)) },
    ns.map{ n => Benchmark(b_koza1, generateTestsU(1, n, fKoza1, -20.0, 20.0)) },
    ns.map{ n => Benchmark(b_squares1, generateTestsU(1, n, fSquares1, -20.0, 20.0)) },
    ns.map{ n => Benchmark(b_squares2_tests, generateTestsU(2, n, fSquares2, -20.0, 20.0)) },
    ns.map{ n => Benchmark(b_squares2_b, generateTestsU(2, n, fSquares2, -20.0, 20.0)) },
    ns.map{ n => Benchmark(b_squares2_s, generateTestsU(2, n, fSquares2, -20.0, 20.0)) },
    ns.map{ n => Benchmark(b_squares2_bs, generateTestsU(2, n, fSquares2, -20.0, 20.0)) },
    ns.map{ n => Benchmark(b_squares3_tests, generateTestsU(3, n, fSquares3, -20.0, 20.0)) },
    ns.map{ n => Benchmark(b_squares3_b, generateTestsU(3, n, fSquares3, -20.0, 20.0)) },
    ns.map{ n => Benchmark(b_squares3_s, generateTestsU(3, n, fSquares3, -20.0, 20.0)) },
    ns.map{ n => Benchmark(b_squares3_bs, generateTestsU(3, n, fSquares3, -20.0, 20.0)) },
    ns.map{ n => Benchmark(b_gravity, generateTestsU(3, n, fGravity, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_gravity_b, generateTestsU(3, n, fGravity, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_gravity_m, generateTestsU(3, n, fGravity, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_gravity_s, generateTestsU(3, n, fGravity, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_gravity_bm, generateTestsU(3, n, fGravity, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_gravity_bs, generateTestsU(3, n, fGravity, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_gravity_ms, generateTestsU(3, n, fGravity, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_gravity_bms, generateTestsU(3, n, fGravity, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_gravityNoG, generateTestsU(3, n, fGravityNoG, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par2, generateTestsU(2, n, fResistancePar2, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par2_c1, generateTestsU(2, n, fResistancePar2, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par2_c2, generateTestsU(2, n, fResistancePar2, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par2_s, generateTestsU(2, n, fResistancePar2, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par2_sc, generateTestsU(2, n, fResistancePar2, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par3, generateTestsU(3, n, fResistancePar3, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par3_c1, generateTestsU(3, n, fResistancePar3, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par3_c2, generateTestsU(3, n, fResistancePar3, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par3_s, generateTestsU(3, n, fResistancePar3, 0.0001, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par3_sc, generateTestsU(3, n, fResistancePar3, 0.0001, 20.0)) }
  ).flatten



  //////////////////////////////////////////////////////////////////////////

  val dirPath = "resources/NRA/gecco19"

  val dir = new File(dirPath)
  if (dir.exists())
    dir.delete()
  dir.mkdir()

  benchmarks.foreach{ b =>
    val sygusCode = RegressionConstraints.generateSygusCode(b, logic="QF_NRA")
    val path = s"${dirPath}/${b.fileName}"
    println(sygusCode)
    println("\n\n")
    Utils.saveFile(path, sygusCode)
  }
}

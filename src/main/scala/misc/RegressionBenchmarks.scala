package misc

import scala.util.Random

object RegressionBenchmarks extends App {
  val rng = Random

  abstract class Property(val name: String)

  case class PropOutputBound(lb: Option[Double], ub: Option[Double], range: Option[(Double, Double)] = None) extends Property("PropOutputBound")
  case class PropAscending(range: Option[(Double, Double)] = None) extends Property("PropAscending")
  case class PropDescending(range: Option[(Double, Double)] = None) extends Property("PropDescending")
  case class PropVarSymmetry(vars: Seq[String], range: Option[(Double, Double)] = None) extends Property("PropVarSymmetry")

  case class Benchmark(name: String, vars: Seq[String], props: Seq[Property],
                       tests: Seq[(Seq[Double], Double)]) {
    def argsSignature: String = vars.map{ v => s"($v Real)" }.mkString("(", "", ")")
  }


  def generateSygusDesc(b: Benchmark): String = {
    var s = "(set-logic QF_NRA)\n"
    s += s"(synth-fun ${b.name} ${b.argsSignature} Real)\n\n"

    b.tests.foreach{ case (in, out) =>
      s += s"(constraint (= (${b.name} ${in.mkString(" ")}) $out))\n"
    }
    s += "\n"

    b.props.foreach{
      case PropOutputBound(lb, ub, range) =>
      case PropAscending(range) =>
      case PropDescending(range) =>
      case PropVarSymmetry(vars, range) =>
    }
    s += "(check-synth)\n"
    s
  }

  def saveFile(path: String, code: String): Unit = {

  }


  def generateTestU(numVars: Int, fun: Seq[Double] => Double,
                    minDouble: Double, maxDouble: Double): (Seq[Double], Double) = {
    def rngDouble() = minDouble + rng.nextDouble() * (maxDouble+1-minDouble)
    val in = 0.until(numVars).map{ i => rngDouble() }
    val out = fun(in)
    (in, out)
  }

  def generateTestsU(numVars: Int, numTests: Int, fun: Seq[Double] => Double,
                     minDouble: Double, maxDouble: Double): Seq[(Seq[Double], Double)] = {
    0.until(numTests).map{ _ => generateTestU(numVars, fun, minDouble, maxDouble) }
  }



  def fGravity(vars: Seq[Double]): Double = vars(0) * vars(1) / vars(2)

  val benchmarks = Seq(
    Benchmark("gravity", Seq("m1", "m2", "r"),
              Seq(PropVarSymmetry(Seq("m1", "m2")), PropOutputBound(Some(0.0), None)),
              generateTestsU(3, 10, fGravity, 0.0, 10.0))
  )




  //////////////////////////////////////////////////////////////////////////

  benchmarks.foreach{ b =>
    println(generateSygusDesc(b))
    println("\n\n")
  }

}

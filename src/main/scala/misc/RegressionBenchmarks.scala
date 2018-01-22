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
    val sfName = b.name
    var s = "(set-logic QF_NRA)\n"
    s += s"(synth-fun $sfName ${b.argsSignature} Real)\n"
    s += b.vars.map{ x => s"(declare-fun $x () Real)" }.mkString("", "\n", "\n")
    s += b.vars.map{ x => s"(declare-fun ${x}_2 () Real)" }.mkString("", "\n", "\n")

    b.tests.foreach{ case (in, out) =>
      s += s"(constraint (= ($sfName ${in.mkString(" ")}) $out))\n"
    }
    s += "\n"

    val constr = b.props.flatMap {
      p => p match {
        case PropOutputBound(lb, ub, range) =>
          var tmp = List[String]()
          if (lb.isDefined)
            tmp = s"(>= ($sfName ${b.vars.mkString(" ")}) ${lb.get})" :: tmp
          if (ub.isDefined)
            tmp = s"(<= ($sfName ${b.vars.mkString(" ")}) ${ub.get})" :: tmp
          tmp
        case PropAscending(range) =>
          var tmp = ""
          if (range.isDefined)
            tmp =

          List()
        case PropDescending(range) => List()
        case PropVarSymmetry(vars, range) => List()
    }}

    s += constr.mkString("(constraint (not (or\n    ", "\n  ", ")))\n")
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



  def fGravity(vars: Seq[Double]): Double = 6.674e-11 * vars(0) * vars(1) / vars(2)

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

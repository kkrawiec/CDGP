package misc

import scala.util.Random

object RegressionBenchmarks extends App {
  val rng = Random

  abstract class Property(val name: String)

  case class PropOutputBound(lb: Option[Double], ub: Option[Double], range: Seq[PropRange] = Seq()) extends Property("PropOutputBound")
  case class PropAscending(range: Seq[PropRange] = Seq()) extends Property("PropAscending")
  case class PropDescending(range: Seq[PropRange] = Seq()) extends Property("PropDescending")
  case class PropVarSymmetry(vars: Seq[String], range: Seq[PropRange] = Seq()) extends Property("PropVarSymmetry")

  case class Benchmark(name: String,
                       vars: Seq[String],
                       props: Seq[Property],
                       tests: Seq[(Seq[Double], Double)]) {
    def argsSignature: String = vars.map{ v => s"($v Real)" }.mkString("(", "", ")")
  }


  abstract class PropRange(varName: String, lb: Option[Double] = None, ub: Option[Double] = None) {
    def getCondition: String = {
      if (lb.isEmpty && ub.isEmpty)
        ""
      else {
        val implCondParts = List((lb, ">="), (ub, "<=")).collect { case (Some(d), sign) => s"($sign $varName $d)" }
        val implCond = if (implCondParts.size > 1) implCondParts.mkString("(and ", " ", ")") else implCondParts.head
        implCond
      }
    }
  }
  case class Range(varName: String, lb: Option[Double] = None, ub: Option[Double] = None) extends PropRange(varName, lb, ub)
  case class EmptyRange() extends PropRange("", None, None)


  def wrapConstrInRanges(constr: String, ranges: Seq[PropRange]): String = {
    if (ranges.isEmpty)
      constr
    else {
      val implCond = ranges.map(_.getCondition).mkString("(and ", " ", ")")
      s"(=> $implCond $constr)"
    }
  }

  def getCodeForProp(b: Benchmark, p: Property): List[String] = {
    val sfName = b.name
    p match {
      case PropOutputBound(lb, ub, range) =>
        var tmp = List[String]()
        if (lb.isDefined) {
          val c = s"(>= ($sfName ${b.vars.mkString(" ")}) ${lb.get})"
          tmp = wrapConstrInRanges(c, range) :: tmp
        }
        if (ub.isDefined) {
          val c = s"(<= ($sfName ${b.vars.mkString(" ")}) ${ub.get})"
          tmp = wrapConstrInRanges(c, range) :: tmp
        }
        tmp
      case PropAscending(range) =>
        var tmp = ""
        List()
      case PropDescending(range) => List()
      case PropVarSymmetry(vars, range) => List()
    }
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


    val constr = b.props.flatMap(getCodeForProp(b, _))

    s += constr.mkString("(constraint (not (or\n    ", "\n    ", ")))\n")
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
              Seq(PropVarSymmetry(Seq("m1", "m2")),
                  PropOutputBound(Some(0.0), None, Seq(Range("m1", lb=Some(0.5), ub=Some(10.0))))),
              generateTestsU(3, 10, fGravity, 0.0, 10.0))
  )




  //////////////////////////////////////////////////////////////////////////

  benchmarks.foreach{ b =>
    println(generateSygusDesc(b))
    println("\n\n")
  }

}

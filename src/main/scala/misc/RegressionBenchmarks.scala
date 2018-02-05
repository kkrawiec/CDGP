package misc

import scala.util.Random

import cdgp.Tools


object RegressionBenchmarks extends App {
  val rng = Random

  abstract class Property(val name: String)


  case class CustomConstraint(formula: String, callMarker: String = "{0}", range: Seq[PropRange] = Seq()) extends Property("CustomConstraint") {
    def getCode(funCall: String) = formula.replace(callMarker, funCall)
  }
  case class PropOutputBound(lb: Option[Double], ub: Option[Double], range: Seq[PropRange] = Seq()) extends Property("PropOutputBound")
  case class PropAscending(range: Seq[PropRange] = Seq()) extends Property("PropAscending")
  case class PropDescending(range: Seq[PropRange] = Seq()) extends Property("PropDescending")
  case class PropVarSymmetry2(var1: String, var2: String, range: Seq[PropRange] = Seq()) extends Property("PropVarSymmetry2")

  case class Benchmark(name: String,
                       vars: Seq[String],
                       props: Seq[Property],
                       tests: Seq[(Seq[Double], Double)]) {
    def argsSignature: String = vars.map{ v => s"($v Real)" }.mkString("(", "", ")")
  }


  abstract class PropRange(varName: String, lb: Option[Double] = None, ub: Option[Double] = None,
                           lbSign: String = ">=", ubSign: String = "<=") {
    assert(lbSign == ">=" || lbSign == ">")
    assert(ubSign == "<=" || ubSign == "<")
    def getCondition: String = {
      if (lb.isEmpty && ub.isEmpty)
        ""
      else {
        val implCondParts = List((lb, lbSign), (ub, ubSign)).collect { case (Some(d), sign) => s"($sign $varName $d)" }
        val implCond = if (implCondParts.size > 1) implCondParts.mkString("(and ", " ", ")") else implCondParts.head
        implCond
      }
    }
  }
  case class Range(varName: String,
                   lb: Option[Double] = None, ub: Option[Double] = None,
                   lbSign: String = ">=", ubSign: String = "<=") extends PropRange(varName, lb, ub, lbSign, ubSign)
  case class EmptyRange() extends PropRange("", None, None)


  def wrapConstrInRanges(constr: String, ranges: Seq[PropRange]): String = {
    if (ranges.isEmpty)
      constr
    else {
      val implCond = ranges.map(_.getCondition).mkString("(and ", " ", ")")
      s"(=> $implCond $constr)"
    }
  }

  def funCall(name: String, vars: Seq[String]): String = s"($name ${vars.mkString(" ")})"

  def getCodeForProp(b: Benchmark, p: Property): List[String] = {
    val sfName = b.name
    var tmp = List[String]()
    p match {

      case PropOutputBound(lb, ub, range) =>
        if (lb.isDefined) {
          val c = s"(>= ${funCall(sfName, b.vars)} ${lb.get})"
          tmp = wrapConstrInRanges(c, range) :: tmp
        }
        if (ub.isDefined) {
          val c = s"(<= ${funCall(sfName, b.vars)} ${ub.get})"
          tmp = wrapConstrInRanges(c, range) :: tmp
        }
        tmp

      case PropAscending(range) =>
        ???

      case cc @ CustomConstraint(_, _, range) =>
        List(wrapConstrInRanges(cc.getCode(funCall(sfName, b.vars)), range))

      case PropDescending(range) =>
        ???

      case PropVarSymmetry2(var1, var2, range) =>
        val i1 = b.vars.indexOf(var1)
        val i2 = b.vars.indexOf(var2)
        val x = b.vars(i1)
        val varsExchanged = b.vars.updated(i1, b.vars(i2)).updated(i2, x)
        val c = s"(= ${funCall(b.name, b.vars)} ${funCall(b.name, varsExchanged)})"
        List(wrapConstrInRanges(c, range))
    }
  }


  def generateConstrTestCases(b: Benchmark): String = {
    val sfName = b.name
    var s = ""
    b.tests.foreach{ case (in, out) =>
      s += s"(constraint (= ($sfName ${in.map(Tools.double2str(_)).mkString(" ")}) ${Tools.double2str(out)}))\n"
    }
    s
  }


  def generateSygusDesc(b: Benchmark): String = {
    val sfName = b.name
    var s = "(set-logic QF_NRA)\n"
    s += s"(synth-fun $sfName ${b.argsSignature} Real)\n"
    // Synthesis variables
    s += b.vars.map{ x => s"(declare-var $x Real)" }.mkString("", "\n", "\n")
    // Some helper variables
    //s += b.vars.map{ x => s"(declare-fun ${x}_2 () Real)" }.mkString("", "\n", "\n")


    s += generateConstrTestCases(b) + "\n"


    val constr = b.props.flatMap(getCodeForProp(b, _))

    s += constr.mkString("(constraint (and\n    ", "\n    ", "))\n")
    s += "(check-synth)\n"
    s
  }

  def saveFile(path: String, code: String): Unit = {

  }


  def generateTestU(numVars: Int, fun: Seq[Double] => Double,
                    minDouble: Double, maxDouble: Double): (Seq[Double], Double) = {
    def rngDouble() = minDouble + rng.nextDouble() * (maxDouble+1-minDouble)
    val in = 0.until(numVars).map{ i => BigDecimal(rngDouble()).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble }
    val out = fun(in)
    (in, out)
  }

  def generateTestsU(numVars: Int, numTests: Int, fun: Seq[Double] => Double,
                     minDouble: Double, maxDouble: Double): Seq[(Seq[Double], Double)] = {
    0.until(numTests).map{ _ => generateTestU(numVars, fun, minDouble, maxDouble) }
  }



  def fGravity(vars: Seq[Double]): Double = 6.674e-11 * vars(0) * vars(1) / (vars(2) * vars(2))
  def fGravityNoG(vars: Seq[Double]): Double = vars(0) * vars(1) / (vars(2) * vars(2))
  def fResistancePar2(vars: Seq[Double]): Double = vars(0) * vars(1) / (vars(0) + vars(1))

  def rangesGZero01(vars: String*): Seq[Range] = vars.map( x => Range(x, lb=Some(0.01), lbSign = ">="))
  def rangesGZero(vars: String*): Seq[Range] = vars.map( x => Range(x, lb=Some(0.0), lbSign = ">"))

  val benchmarks = Seq(
//    Benchmark("gravity", Seq("m1", "m2", "r"),
//      Seq(
//        PropVarSymmetry2("m1", "m2", rangesGZero01("m1", "m2", "r")),
//        PropOutputBound(Some(0.0), None, rangesGZero01("m1", "m2", "r"))
//      ),
//      generateTestsU(3, 25, fGravity, 0.0, 20.0)),

    Benchmark("gravity_noG", Seq("m1", "m2", "r"),
      Seq(
        PropVarSymmetry2("m1", "m2", rangesGZero01("m1", "m2", "r")),
        PropOutputBound(Some(0.0), None, rangesGZero01("m1", "m2", "r"))
      ),
      generateTestsU(3, 25, fGravityNoG, 0.0, 20.0)),

    // task: calculate the total resistance of 2 parallel resistors
    Benchmark("resistance_par2", Seq("r1", "r2"),
      Seq(
        PropVarSymmetry2("r1", "r2", rangesGZero("r1", "r2")),
        CustomConstraint("(>= {0} (+ r1 r2))", range=rangesGZero("r1", "r2"))
      ),
      generateTestsU(2, 25, fResistancePar2, 0.0, 20.0))
  )




  //////////////////////////////////////////////////////////////////////////

  benchmarks.foreach{ b =>
    println(generateSygusDesc(b))
    println("\n\n")
  }

}

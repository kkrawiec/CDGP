package misc

import java.io.{BufferedWriter, File, FileWriter}

import scala.util.Random
import cdgp.Tools


object RegressionBenchmarks extends App {
  val rng = Random

  case class Benchmark(name: String,
                       vars: Seq[String],
                       props: Seq[Property],
                       tests: Seq[(Seq[Double], Double)] = Seq()) {
    def fileName: String = name + "_" + tests.size + ".sl"
    def argsSignature: String = vars.map{ v => s"($v Real)" }.mkString("(", "", ")")
  }

  object Benchmark {
    def apply(b: Benchmark,
              tests: Seq[(Seq[Double], Double)]): Benchmark = new Benchmark(b.name, b.vars, b.props, tests)
  }



  abstract class Property(val name: String) {
    /**
      * Returns an SMT-LIB encoding of the property given the concrete benchmark instance.
      */
    def encode(b: Benchmark): Seq[String]

    /**
      * If ranges are defined, then the given encoding of the property will be wrapped in
      * implication with ranges in it's condition part.
      */
    def wrapConstrInRanges(constr: String, ranges: Seq[VarRange]): String = {
      if (ranges.isEmpty)
        constr
      else {
        val implCond = ranges.map(_.getCondition).mkString("(and ", " ", ")")
        s"(=> $implCond $constr)"
      }
    }
  }


  /**
    * Allows expression of arbitrary constraints.
    *
    * @param formula A formula representing the constraint.
    * @param callMarker Every instance of callMarker string in the formula will be replaced with
    *                   synth-fun call or expr parameter, if any was provided.
    * @param range Applicability range of this constraint in terms of variables.
    * @param expr Expression which will be put into the constraint.
    */
  case class CustomConstraint(formula: String, callMarker: String = "{0}", range: Seq[VarRange] = Seq(), expr: String = "") extends Property("CustomConstraint") {
    override def encode(b: Benchmark): Seq[String] = {
      val expression = if (expr != "") expr else funCall(b.name, b.vars)
      List(wrapConstrInRanges(formula.replace(callMarker, expression), range))
    }
  }


  case class PropOutputBound(lb: Option[Double], ub: Option[Double],
                             lbSign: String = ">=", ubSign: String = "<=",
                             range: Seq[VarRange] = Seq()) extends Property("PropOutputBound") {
    assert(List(">=", ">", "=", "distinct").contains(lbSign))
    assert(List("<=", "<", "=", "distinct").contains(ubSign))
    override def encode(b: Benchmark): Seq[String] = {
      val sfName = b.name
      var tmp = List[String]()
      if (lb.isDefined) {
        val c = s"($lbSign ${funCall(sfName, b.vars)} ${lb.get})"
        tmp = wrapConstrInRanges(c, range) :: tmp
      }
      if (ub.isDefined) {
        val c = s"($ubSign ${funCall(sfName, b.vars)} ${ub.get})"
        tmp = wrapConstrInRanges(c, range) :: tmp
      }
      tmp
    }
  }


  case class PropAscending(range: Seq[VarRange] = Seq()) extends Property("PropAscending") {
    override def encode(b: Benchmark): Seq[String] = ???
  }


  case class PropDescending(range: Seq[VarRange] = Seq()) extends Property("PropDescending") {
    override def encode(b: Benchmark): Seq[String] = ???
  }


  case class PropVarSymmetry2(var1: String, var2: String, range: Seq[VarRange] = Seq())
    extends Property("PropVarSymmetry2") {

    override def encode(b: Benchmark): Seq[String] = {
      val i1 = b.vars.indexOf(var1)
      val i2 = b.vars.indexOf(var2)
      assert(i1 != -1 && i2 != -1)
      val x = b.vars(i1)
      val varsExchanged = b.vars.updated(i1, b.vars(i2)).updated(i2, x)
      val c = s"(= ${funCall(b.name, b.vars)} ${funCall(b.name, varsExchanged)})"
      List(wrapConstrInRanges(c, range))
    }
  }


  /**
    * A range of possible values for a certain variable. Any property can be specified to work
    * only for variables in a certain range, which is defined using this class.
    *
    * @param varName Name of the variable.
    * @param lb Lower bound value.
    * @param ub Upper bound value.
    * @param lbSign <= (default) or <.
    * @param ubSign >= (default) or >.
    */
  abstract class VarRange(varName: String, lb: Option[Double] = None, ub: Option[Double] = None,
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
  case class EmptyRange() extends VarRange("", None, None)
  case class Range(varName: String, lb: Option[Double] = None, ub: Option[Double] = None,
                   lbSign: String = ">=", ubSign: String = "<=") extends VarRange(varName, lb, ub, lbSign, ubSign)
  case class RangeLU(varName: String, lb: Double, ub: Double,
                     lbSign: String = ">=", ubSign: String = "<=") extends VarRange(varName, Some(lb), Some(ub), lbSign, ubSign)




  def funCall(name: String, vars: Seq[String]): String = s"($name ${vars.mkString(" ")})"


  def generateConstrTestCases(b: Benchmark): String = {
    val sfName = b.name
    var s = ""
    b.tests.foreach{ case (in, out) =>
      s += s"(constraint (= ($sfName ${in.map(Tools.double2str(_)).mkString(" ")}) ${Tools.double2str(out)}))\n"
    }
    s
  }


  def generateSygusCode(b: Benchmark): String = {
    val sfName = b.name
    var s = "(set-logic QF_NRA)\n"
    s += s"(synth-fun $sfName ${b.argsSignature} Real)\n"
    // Synthesis variables
    s += b.vars.map{ x => s"(declare-var $x Real)" }.mkString("", "\n", "\n")
    // Some helper variables
    //s += b.vars.map{ x => s"(declare-fun ${x}_2 () Real)" }.mkString("", "\n", "\n")

    s += generateConstrTestCases(b) + "\n"

    val constr = b.props.flatMap(_.encode(b))

    s += constr.mkString("(constraint (and\n    ", "\n    ", "))\n")
    s += "(check-synth)\n"
    s
  }

  def saveFile(path: String, text: String): Unit = {
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
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


  val b_gravity = Benchmark("gravity", Seq("m1", "m2", "r"),
    Seq(
      PropVarSymmetry2("m1", "m2", rangesGZero01("m1", "m2", "r")),
      PropOutputBound(Some(0.0), None, range=rangesGZero01("m1", "m2", "r"))
    ))
  val b_gravityNoG = Benchmark("gravity_noG", Seq("m1", "m2", "r"),
    Seq(
      PropVarSymmetry2("m1", "m2", range=rangesGZero01("m1", "m2", "r")),
      PropOutputBound(Some(0.0), None, range=rangesGZero01("m1", "m2", "r"))
    ))
  // task: calculate the total resistance of 2 parallel resistors
  val b_resistance_par2 = Benchmark("resistance_par2", Seq("r1", "r2"),
    Seq(
      PropVarSymmetry2("r1", "r2", rangesGZero("r1", "r2")),
      CustomConstraint("(and (<= {0} r1) (<= {0} r2))", range=rangesGZero("r1", "r2"))
    ))

  val ns = Seq(10, 25, 50)

  val benchmarks = Seq(
    ns.map{ n => Benchmark(b_gravity, generateTestsU(3, n, fGravity, 0.0, 20.0)) },
    ns.map{ n => Benchmark(b_gravityNoG, generateTestsU(3, n, fGravityNoG, 0.0, 20.0)) },
    ns.map{ n => Benchmark(b_resistance_par2, generateTestsU(2, n, fResistancePar2, 0.0, 20.0)) }
  ).flatten



  //////////////////////////////////////////////////////////////////////////

  val dir = new File("resources/NRA/regression_props")
  if (dir.exists())
    dir.delete()
  dir.mkdir()

  benchmarks.foreach{ b =>
    val sygusCode = generateSygusCode(b)
    val path = "resources/NRA/regression_props/" + b.fileName
    println(sygusCode)
    println("\n\n")
    saveFile(path, sygusCode)
  }

}

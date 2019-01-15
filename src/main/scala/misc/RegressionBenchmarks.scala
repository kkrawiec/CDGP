package misc

import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable
import scala.util.Random
import cdgp.Tools


object RegressionBenchmarks extends App {
  val rng = Random

  /**
    * Description of a benchmark.
    *
    * @param funName Name of the function.
    * @param vars Name of the function's arguments.
    * @param props List of the function properties.
    * @param tests List of input-output test cases function is supposed to pass.
    */
  case class Benchmark(funName: String,
                       vars: Seq[String],
                       props: Seq[Property],
                       tests: Seq[(Seq[Double], Double)] = Seq()) {
    def fileName: String = funName + "_" + tests.size + ".sl"
    def argsSignature: String = vars.map{ v => s"($v Real)" }.mkString("(", "", ")")
  }

  object Benchmark {
    def apply(b: Benchmark,
              tests: Seq[(Seq[Double], Double)]): Benchmark = new Benchmark(b.funName, b.vars, b.props, tests)
  }



  abstract class Property(val name: String) {
    /**
      * Returns a tuple containing:
      * 1) A list of necessary SMT-LIB declarations.
      * 2) SMT-LIB encoding of this property.
      *
      * @param id identifier of the property.
      * @param b all data regarding some benchmark.
      */
    def encode(id: Int, b: Benchmark): (Seq[String], Seq[String])

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

    def prefixedName(i: Int, name: String): String = s"cdgp.P$i.$name"
  }

  abstract class PropertyMonotonicity(name: String) extends Property(name) {
    /** Every cdgp.Pi.var1 variable should be bounded by the same range as var1 variable. */
    def updateRange(range: Seq[VarRange], var1: String, id: Int): Seq[VarRange] = {
      val rv = range.find{ case Range(v, _, _, _, _) => v == var1 }
      if (rv.isDefined) {
        val rv2 = rv.get match {
          case Range(v, lb, ub, lbS, ubS) => Range(prefixedName(id, var1), lb, ub, lbS, ubS)
          case _ => throw new Exception("Invalid occurrence of an empty range!")
        }
        range.+:(rv2)
      }
      else
        range
    }
  }


  /**
    * Arbitrary constraint provided as an SMT-LIB formula.
    *
    * @param formula A formula representing the constraint.
    * @param callMarker Every instance of callMarker string in the formula will be replaced with
    *                   synth-fun call or expr parameter, if any was provided.
    * @param range Applicability range of this constraint in terms of variables.
    * @param expr Expression which will be put into the constraint.
    */
  case class CustomConstraint(formula: String, callMarker: String = "{0}", range: Seq[VarRange] = Seq(), expr: String = "")
    extends Property("CustomConstraint") {

    override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
      val expression = if (expr != "") expr else funCall(b.funName, b.vars)
      (List(), List(wrapConstrInRanges(formula.replace(callMarker, expression), range)))
    }
  }


  /**
    * Produces constraint stating that the function's output is always within certain bounds.
    * (assert (>= (f x) 0.0))
    */
  case class PropOutputBound(lb: Option[Double], ub: Option[Double],
                             lbSign: String = ">=", ubSign: String = "<=",
                             range: Seq[VarRange] = Seq())
    extends Property("PropOutputBound") {
    assert(List(">=", ">", "=", "distinct").contains(lbSign))
    assert(List("<=", "<", "=", "distinct").contains(ubSign))
    override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
      val sfName = b.funName
      var tmp = List[String]()
      if (lb.isDefined) {
        val c = s"($lbSign ${funCall(sfName, b.vars)} ${lb.get})"
        tmp = wrapConstrInRanges(c, range) :: tmp
      }
      if (ub.isDefined) {
        val c = s"($ubSign ${funCall(sfName, b.vars)} ${ub.get})"
        tmp = wrapConstrInRanges(c, range) :: tmp
      }
      (List(), tmp)
    }
  }


  /**
    * Produces constraint that is used to guarantee that the function is always ascending with the
    * change on the var1 variable. In SMT-LIB this looks like this:
    * (declare-fun var1 () Real)
    * (declare-fun cdgp.P1.var1 () Real)
    * (assert (=> (> cdgp.P1.var1 var1)  (>= (f cdgp.P1.var1) (f var1))))
    *
    * NOTE: the constraint presented above is assumed to be negated during verification. Normally,
    * a universal quantifier on the cdgp.P1.var1 should be used in the assertion above. But in
    * such a case a counterexample would not be produced, because cdgp.P1.var1 would be a bounded
    * variable.
    */
  case class PropAscending(var1: String, range: Seq[VarRange] = Seq(), strict: Boolean = false)
    extends PropertyMonotonicity("PropAscending") {

    override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
      val var1Prop = prefixedName(id, var1)
      val decls = List(s"(declare-fun $var1Prop () Real)")
      val sign = if (strict) ">" else ">="
      val varsChanged = changeVarNames(b.vars, Map(var1->var1Prop))
      val c = s"(=> (> $var1Prop $var1)  ($sign ${funCall(b.funName, varsChanged)} ${funCall(b.funName, b.vars)}))"
      val constr = List(wrapConstrInRanges(c, updateRange(range, var1, id)))
      (decls, constr)
    }
  }

  case class PropDescending(var1: String, range: Seq[VarRange] = Seq(), strict: Boolean = false)
    extends PropertyMonotonicity("PropDescending") {

    override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
      val var1Prop = prefixedName(id, var1)
      val decls = List(s"(declare-fun $var1Prop () Real)")
      val sign = if (strict) "<" else "<="
      val varsChanged = changeVarNames(b.vars, Map(var1->var1Prop))
      val c = s"(=> (> $var1Prop $var1)  ($sign ${funCall(b.funName, varsChanged)} ${funCall(b.funName, b.vars)}))"
      val constr = List(wrapConstrInRanges(c, updateRange(range, var1, id)))
      (decls, constr)
    }
  }





  /**
    * Produces constraint for a value of a derivative in a certain point:
    * (assert (not (=> (and (> x (- dp1 eps1))  (< x (+ dp1 eps1)))
    *   (and (> (/ (- (f (+ x eps2)) (f x) ) eps2) (- derivative eps3) )
    *        (< (/ (- (f (+ x eps2)) (f x) ) eps2) (+ derivative eps3) )
    *   )
    * )))
    *
    * @param dVar Variable, for which derivative is computed.
    * @param x Point for which expected derivative is specified.
    * @param degree Degree of the derivation, e.g. degree=2 is the second derivative.
    * @param expDerivative Expected value of the derivative of synthesized function in point x.
    * @param dNeigh The neighbourhood of the point which is supposed to have a similar derivative. (eps1 in the script)
    * @param dDelta Derivative will be computed between points x and (x+eps2). (eps2 in the script)
    * @param dValue Acceptable difference from the expected value of the derivative. (eps3 in the script)
    * @param range Range of variables for which constraint is intended to be satisfied.
    */
  case class PropApproxDerivative(dVar: String,
                                  x: Double,
                                  expDerivative: Double,
                                  degree: Int = 1,
                                  dNeigh: Double = 0.0,
                                  dDelta: Double = 0.000001,
                                  dValue: Double = 0.001,
                                  range: Seq[VarRange] = Seq())
    extends Property("PropApproxDerivative") {
    assert(degree > 0)
    assert(dNeigh >= 0.0)
    assert(dDelta >= 0.0)
    assert(dValue >= 0.0)
    override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
      val sfName = b.funName
      var decls = List[String]()

      val derivative = getDerivative(b)
      val text = "(<= (abs (- %1$s %2$s)) %3$s)".format(derivative, Tools.double2str(expDerivative), Tools.double2str(dValue))

      val range = getNeighbourhoodRange  // range for derivative neighbourhood
      if (range.isDefined)
        (decls, List(wrapConstrInRanges(text, Seq(range.get))))
      else
        (decls, List(text))
    }

    def getNeighbourhoodRange: Option[VarRange] = {
      val r = Range(dVar, lb=Some(x-dNeigh), ub=Some(x+dNeigh), lbSign=">=", ubSign="<=")
      Some(r)
    }

    def getDerivative(b: Benchmark): String = {
      def getFunCallArgs(no_h: Int) = {
        if (no_h == 0) dVar
        else if (no_h == 1) s"(+ $dVar ${Tools.double2str(dDelta)})"
        else s"(+ $dVar (* ${Tools.double2str(no_h)} ${Tools.double2str(dDelta)}))"
      }
      def getH: String = {
        if (degree == 1) s"${Tools.double2str(dDelta)}" else f"(*${s" ${Tools.double2str(dDelta)}" * degree})"
      }
      def getBody(k: Int, no_h: Int): String = {
        if (k == 0) {
          val callArgs = b.vars.map{ x => if (x == dVar) getFunCallArgs(no_h) else x }
          s"${funCall(b.funName, callArgs)}"
        }
        else {
          val a = getBody(k-1, no_h+1)
          val b = getBody(k-1, no_h)
          s"(- $a $b)"
        }
      }
      val h = getH
      val body = getBody(degree, 0)
      s"(/ $body $h)"
    }
  }


  /**
    * Produces constraint indicating that the result of a function is the same under exchanging the
    * positions of the specified variables.
    * (= (f x y) (f y x))
    */
  case class PropVarSymmetry2(var1: String, var2: String, range: Seq[VarRange] = Seq())
    extends Property("PropVarSymmetry2") {

    override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
      val i1 = b.vars.indexOf(var1)
      val i2 = b.vars.indexOf(var2)
      assert(i1 != -1 && i2 != -1, "Incorrect variable name!")
      val x = b.vars(i1)
      val varsExchanged = b.vars.updated(i1, b.vars(i2)).updated(i2, x)
      val c = s"(= ${funCall(b.funName, b.vars)} ${funCall(b.funName, varsExchanged)})"
      (List(), List(wrapConstrInRanges(c, range)))
    }
  }


  /**
    * Produces constraint indicating that the result of a function does not change when the
    * specified var1 variable is negated, i.e. f(x,y) = f(-x,y).
    * (= (f x y) (f (- x) y))
    */
  case class PropSymmetryYAxis(var1: String, range: Seq[VarRange] = Seq())
    extends Property("PropSymmetryYAxis") {

    override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
      val i1 = b.vars.indexOf(var1)
      assert(i1 != -1, "Incorrect variable name!")
      val x = b.vars(i1)
      val varsExchanged = b.vars.updated(i1, s"(- ${b.vars(i1)})")
      val c = s"(= ${funCall(b.funName, b.vars)} ${funCall(b.funName, varsExchanged)})"
      (List(), List(wrapConstrInRanges(c, range)))
    }
  }



  /**
    * Produces constraint indicating that the function is injective, i.e. a function f
    * won't produce the same outcome for two different arguments.
    * (declare-fun cdgp.P1.x () Real)
    * (declare-fun cdgp.P1.y () Real)
    * (assert (=> (and (distinct x cdgp.P1.x) (distinct y cdgp.P1.y))
    *             (distinct (f2 x y) (f2 cdgp.P1.x cdgp.P1.y))))
    */
  case class PropInjective(range: Seq[VarRange] = Seq())
    extends Property("PropInjective") {

    override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
      val prefixedVars = b.vars.map(prefixedName(id, _))
      val decls = prefixedVars.map{ v => s"(declare-fun $v () Real)" }
      val c = s"(=> ${antecedent(b, prefixedVars)} ${consequent(b, prefixedVars)})"
      (decls, List(wrapConstrInRanges(c, updateRange(range, prefixedVars, id))))
    }

    def antecedent(b: Benchmark, prefixedVars: Seq[String]): String = {
      val conds = b.vars.zip(prefixedVars).map{ case (v1, v2) => s"(distinct $v1 $v2)" }
      s"(and ${conds.mkString("", " ", "")})"
    }

    def consequent(b: Benchmark, prefixedVars: Seq[String]): String = {
      s"(distinct ${funCall(b.funName, b.vars)} ${funCall(b.funName, prefixedVars)})"
    }

    /** Every cdgp.Pi.x variable should be bounded by the same range as x variable. */
    def updateRange(range: Seq[VarRange], prefixedVars: Seq[String], id: Int): Seq[VarRange] = {
      range.zip(prefixedVars).flatMap{
        case (r @ Range(v, lb, ub, lbS, ubS), newV) => List(r, Range(newV, lb, ub, lbS, ubS))
      }
    }
  }



  /**
    * Negates the provided property.
    *
    * NOTE: must be used very carefully! Particularly, if the constraint is in the form
    * of an implication, then negating it and then negating it again during verification
    * will create a situation, in which it simply suffices to make the antecedent false
    * (which usually is trivial).
    */
  case class PropNot(prop: Property, range: Seq[VarRange] = Seq())
    extends Property("PropNot") {

    override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
      val (decls, constr) = prop.encode(id, b)
      (decls, constr.map(c => s"(not $c)"))
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
  abstract class VarRange(val varName: String, val lb: Option[Double] = None, val ub: Option[Double] = None,
                          val lbSign: String = ">=", val ubSign: String = "<=") {
    assert(lbSign == ">=" || lbSign == ">")
    assert(ubSign == "<=" || ubSign == "<")
    def getCondition: String = {
      if (lb.isEmpty && ub.isEmpty)
        ""
      else if (lb.isDefined && ub.isDefined && lb.get == ub.get &&
               lbSign == ">=" && ubSign == "<=") {
        s"(= $varName ${lb.get})"
      }
      else {
        val implCondParts = List((lb, lbSign), (ub, ubSign)).collect { case (Some(d), sign) => s"($sign $varName $d)" }
        val implCond = if (implCondParts.size > 1) implCondParts.mkString("(and ", " ", ")") else implCondParts.head
        implCond
      }
    }
  }
  case class EmptyRange() extends VarRange("", None, None)
  case class Range(override val varName: String,
                   override val lb: Option[Double] = None,
                   override val ub: Option[Double] = None,
                   override val lbSign: String = ">=",
                   override val ubSign: String = "<=") extends VarRange(varName, lb, ub, lbSign, ubSign)



  // Helper functions
  //--------------------------------------------
  def changeVarNames(b: Benchmark, changes: Map[String, String]): Seq[String] =
    changeVarNames(b.vars, changes)
  def changeVarNames(vars: Seq[String], changes: Map[String, String]): Seq[String] = {
    var res = vars
    changes.foreach { c: (String, String) =>
      val i = vars.indexOf(c._1)
      assert(i != -1)
      res = res.updated(i, c._2)
    }
    res
  }
  def funCall(b: Benchmark): String = funCall(b.funName, b.vars)
  def funCall(name: String, vars: Seq[String]): String = s"($name ${vars.mkString(" ")})"

  //--------------------------------------------


  def generateConstrTestCases(b: Benchmark): String = {
    val sfName = b.funName
    var s = ""
    b.tests.foreach{ case (in, out) =>
      s += s"(constraint (= ($sfName ${in.map(Tools.double2str(_)).mkString(" ")}) ${Tools.double2str(out)}))\n"
    }
    s
  }


  def generateSygusCode(b: Benchmark, mergeFormalConstr: Boolean = false): String = {
    val sfName = b.funName
    var s = "(set-logic QF_NRA)\n"
    s += s"(synth-fun $sfName ${b.argsSignature} Real)\n"
    // Synthesis variables
    s += b.vars.map{ x => s"(declare-var $x Real)" }.mkString("", "\n", "\n")
    // Some helper variables
    //s += b.vars.map{ x => s"(declare-fun ${x}_2 () Real)" }.mkString("", "\n", "\n")

    s += generateConstrTestCases(b) + "\n"


    val decls = mutable.ListBuffer[String]()
    val constr = mutable.ListBuffer[String]()

    b.props.indices.zip(b.props).foreach{ case (i, prop) =>
      val (d, c) = prop.encode(i, b)
      decls ++= d
      constr ++= c
    }

    s += decls.mkString("\n")
    s += "\n\n"
    if (mergeFormalConstr)
      s += constr.mkString("(constraint (and\n    ", "\n    ", "))")
    else
      s += constr.map( c => s"(constraint $c)").mkString("\n")
    s += "\n\n(check-synth)\n"
    s
  }

  def saveFile(path: String, text: String): Unit = {
    try {
      val file = new File(path)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(text)
      bw.close()
    }
    catch {
      case e: Exception => println(s"Error while saving file $path.\nError:\n${e.getMessage}")
    }
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



  def fPoly1(vars: Seq[Double]): Double = vars(0) * vars(0)
  def fPoly2(vars: Seq[Double]): Double = vars(0) * vars(0) + vars(1) * vars(1)
  def fGravity(vars: Seq[Double]): Double = 6.674e-11 * vars(0) * vars(1) / (vars(2) * vars(2))
  def fGravityNoG(vars: Seq[Double]): Double = vars(0) * vars(1) / (vars(2) * vars(2))
  def fResistancePar2(vars: Seq[Double]): Double = vars(0) * vars(1) / (vars(0) + vars(1))

  def rangesGeqZero01(vars: String*): Seq[Range] = vars.map(x => Range(x, lb=Some(0.01), lbSign = ">="))
  def rangesGtZero(vars: String*): Seq[Range] = vars.map(x => Range(x, lb=Some(0.0), lbSign = ">"))
  def rangesLeqZero01(vars: String*): Seq[Range] = vars.map(x => Range(x, ub=Some(-0.01), ubSign = "<="))
  def rangesLtZero(vars: String*): Seq[Range] = vars.map(x => Range(x, ub=Some(0.0), ubSign = "<"))


  val b_poly1 = Benchmark("poly1", Seq("x"),
    Seq(
      PropApproxDerivative("x", 1.0, 2.0, degree=1),
      PropApproxDerivative("x", 1.0, 2.0, degree=2),
      PropApproxDerivative("x", 1.0, 0.0, degree=3),
      PropSymmetryYAxis("x"),
      PropOutputBound(Some(0.0), None),
      PropAscending("x", range=rangesGtZero("x")),
      PropDescending("x", range=rangesLtZero("x"))
    ))
  val b_poly2 = Benchmark("poly2", Seq("x", "y"),
    Seq(
      PropApproxDerivative("x", 1.0, 2.0, degree=1),
      PropApproxDerivative("x", 1.0, 2.0, degree=2),
      PropApproxDerivative("x", 1.0, 0.0, degree=3),
      PropSymmetryYAxis("x"),
      PropOutputBound(Some(0.0), None),
      PropAscending("x", range=rangesGtZero("x")),
      PropDescending("x", range=rangesLtZero("x"))
    ))
  val b_gravity = Benchmark("gravity", Seq("m1", "m2", "r"),
    Seq(
      PropVarSymmetry2("m1", "m2", rangesGeqZero01("m1", "m2", "r")),
      PropOutputBound(Some(0.0), None, range=rangesGeqZero01("m1", "m2", "r")),
      PropAscending("m1", range=rangesGeqZero01("m1", "m2", "r")),
      PropAscending("m2", range=rangesGeqZero01("m1", "m2", "r"))
    ))
  val b_gravityNoG = Benchmark("gravity_noG", Seq("m1", "m2", "r"),
    Seq(
      PropVarSymmetry2("m1", "m2", range=rangesGeqZero01("m1", "m2", "r")),
      PropOutputBound(Some(0.0), None, range=rangesGeqZero01("m1", "m2", "r")),
      PropAscending("m1", range=rangesGeqZero01("m1", "m2", "r")),
      PropAscending("m2", range=rangesGeqZero01("m1", "m2", "r"))
    ))
  // task: calculate the total resistance of 2 parallel resistors
  val b_resistance_par2 = Benchmark("resistance_par2", Seq("r1", "r2"),
    Seq(
      PropVarSymmetry2("r1", "r2", rangesGtZero("r1", "r2")),
      CustomConstraint("(and (<= {0} r1) (<= {0} r2))", range=rangesGtZero("r1", "r2"))
    ))

  val ns = Seq(10, 25, 50)

  val benchmarks = Seq(
    //ns.map{ n => Benchmark(b_poly1, generateTestsU(1, n, fPoly1, 0.0, 20.0)) },
    ns.map{ n => Benchmark(b_poly2, generateTestsU(2, n, fPoly2, 0.0, 20.0)) },
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

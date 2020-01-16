package misc

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import cdgp.Tools


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
  def fileName(extension: String): String = funName + "_" + tests.size + extension
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
    * implication with ranges in its condition part.
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
    val rv = range.find{ case Range(v, _, _, _, _, _) => v == var1 }
    if (rv.isDefined) {
      val rv2 = rv.get match {
        case Range(v, lb, ub, lbS, ubS, oper) => Range(prefixedName(id, var1), lb, ub, lbS, ubS, oper)
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
    val expression = if (expr != "") expr else RegressionConstraints.funCall(b.funName, b.vars)
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
      val c = s"($lbSign ${RegressionConstraints.funCall(sfName, b.vars)} ${lb.get})"
      tmp = wrapConstrInRanges(c, range) :: tmp
    }
    if (ub.isDefined) {
      val c = s"($ubSign ${RegressionConstraints.funCall(sfName, b.vars)} ${ub.get})"
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
  * NOTE: this constraint is able to produce counterexamples, but they are essentially rather
  * uninformative, since not necessarily the output for that counterexample was incorrect.
 *  This constraint SHOULD NOT be used as a partial constraint, because the checking of
 *  correctness will yield incorrect results (since forall was abondoned).
  */
@deprecated("This version of monotonicity constraint not always behaves as it should. Please rather use PropAscending, unless you know what you are doing.")
case class PropAscendingFreeVars(var1: String, range: Seq[VarRange] = Seq(), strict: Boolean = false)
  extends PropertyMonotonicity("PropAscendingFreeVars") {

  override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
    val var1Prop = prefixedName(id, var1)
    val decls = List(s"(declare-fun $var1Prop () Real)")
    val sign = if (strict) ">" else ">="
    val varsChanged = RegressionConstraints.changeVarNames(b.vars, Map(var1->var1Prop))
    val c = s"(=> (> $var1Prop $var1)  ($sign ${RegressionConstraints.funCall(b.funName, varsChanged)} ${RegressionConstraints.funCall(b.funName, b.vars)}))"
    val constr = List(wrapConstrInRanges(c, updateRange(range, var1, id)))
    (decls, constr)
  }
}

@deprecated("This version of monotonicity constraint not always behaves as it should. Please rather use PropDescending, unless you know what you are doing.")
case class PropDescendingFreeVars(var1: String, range: Seq[VarRange] = Seq(), strict: Boolean = false)
  extends PropertyMonotonicity("PropDescendingFreeVars") {

  override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
    val var1Prop = prefixedName(id, var1)
    val decls = List(s"(declare-fun $var1Prop () Real)")
    val sign = if (strict) "<" else "<="
    val varsChanged = RegressionConstraints.changeVarNames(b.vars, Map(var1->var1Prop))
    val c = s"(=> (> $var1Prop $var1)  ($sign ${RegressionConstraints.funCall(b.funName, varsChanged)} ${RegressionConstraints.funCall(b.funName, b.vars)}))"
    val constr = List(wrapConstrInRanges(c, updateRange(range, var1, id)))
    (decls, constr)
  }
}


/**
 * Produces constraint that is used to guarantee that the function is always ascending with the
 * change on the var1 variable. In SMT-LIB this looks like this:
 * (define-fun f ((var1 Real)(var2 Real)) Real ...)
 * (assert (forall ((var1 Real)(var2 Real)(cdgp.P1.x Real))
 *     (=> (> cdgp.P1.var1 var1)  (>= (f cdgp.P1.var1 var2) (f var1 var2)))
 * ))
 *
 * NOTE: this constraint is not able to produce counterexamples. It can, however, be useful as
 * a partial constraint in the fitness vector.
 */
case class PropAscending(var1: String, range: Seq[VarRange] = Seq(), strict: Boolean = false)
  extends PropertyMonotonicity("PropAscending") {

  def getForallVariablesList(var1Prop: String, b: Benchmark): String = {
    val v1 = b.vars.mkString("(", " Real)(", " Real)")
    val v2 = s"($var1Prop Real)"
    s"(${v1}${v2})"
  }

  override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
    val var1Prop = prefixedName(id, var1)
    val decls = List()
    val sign = if (strict) ">" else ">="
    val varsChanged = RegressionConstraints.changeVarNames(b.vars, Map(var1->var1Prop))
    val c = s"(=> (> $var1Prop $var1)  ($sign ${RegressionConstraints.funCall(b.funName, varsChanged)} ${RegressionConstraints.funCall(b.funName, b.vars)}))"
    val cRange = wrapConstrInRanges(c, updateRange(range, var1, id))
    val constr = List(s"(forall ${getForallVariablesList(var1Prop, b)} $cRange)")
    (decls, constr)
  }
}


case class PropDescending(var1: String, range: Seq[VarRange] = Seq(), strict: Boolean = false)
  extends PropertyMonotonicity("PropDescending") {

  def getForallVariablesList(var1Prop: String, b: Benchmark): String = {
    val v1 = b.vars.mkString("(", " Real)(", " Real)")
    val v2 = s"($var1Prop Real)"
    s"(${v1}${v2})"
  }

  override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
    val var1Prop = prefixedName(id, var1)
    val decls = List()
    val sign = if (strict) "<" else "<="
    val varsChanged = RegressionConstraints.changeVarNames(b.vars, Map(var1->var1Prop))
    val c = s"(=> (> $var1Prop $var1)  ($sign ${RegressionConstraints.funCall(b.funName, varsChanged)} ${RegressionConstraints.funCall(b.funName, b.vars)}))"
    val cRange = wrapConstrInRanges(c, updateRange(range, var1, id))
    val constr = List(s"(forall ${getForallVariablesList(var1Prop, b)} $cRange)")
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
        s"${RegressionConstraints.funCall(b.funName, callArgs)}"
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
    assert(i1 != -1 || i2 != -1, "Incorrect variable name!")
    val x = b.vars(i1)
    val varsExchanged = b.vars.updated(i1, b.vars(i2)).updated(i2, x)
    val c = s"(= ${RegressionConstraints.funCall(b.funName, b.vars)} ${RegressionConstraints.funCall(b.funName, varsExchanged)})"
    (List(), List(wrapConstrInRanges(c, range)))
  }
}


/**
  * Produces constraint indicating that the result of a function is the same under exchanging the
  * positions of the specified variables.
  * (and  (= (f x y z) (f y x z))  (= (f x y z) (f z y x))  (= (f x y z) (f x z y)) )
  */
case class PropVarSymmetry3(var1: String, var2: String, var3: String, range: Seq[VarRange] = Seq())
  extends Property("PropVarSymmetry3") {

  override def encode(id: Int, b: Benchmark): (Seq[String], Seq[String]) = {
    val i1 = b.vars.indexOf(var1)
    val i2 = b.vars.indexOf(var2)
    val i3 = b.vars.indexOf(var3)
    assert(i1 != -1 || i2 != -1 || i3 != -1, "Incorrect variable name!")
    val x = b.vars(i1)
    val y = b.vars(i2)
    val z = b.vars(i3)
    val varsExchanged1 = b.vars.updated(i1, y).updated(i2, x)
    val varsExchanged2 = b.vars.updated(i1, z).updated(i3, x)
    val varsExchanged3 = b.vars.updated(i2, z).updated(i3, y)
    val mainCall = RegressionConstraints.funCall(b.funName, b.vars)
    val eq1 = s"(= $mainCall ${RegressionConstraints.funCall(b.funName, varsExchanged1)})"
    val eq2 = s"(= $mainCall ${RegressionConstraints.funCall(b.funName, varsExchanged2)})"
    val eq3 = s"(= $mainCall ${RegressionConstraints.funCall(b.funName, varsExchanged3)})"
    val c = s"(and  $eq1  $eq2  $eq3)"
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
    val c = s"(= ${RegressionConstraints.funCall(b.funName, b.vars)} ${RegressionConstraints.funCall(b.funName, varsExchanged)})"
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
    s"(distinct ${RegressionConstraints.funCall(b.funName, b.vars)} ${RegressionConstraints.funCall(b.funName, prefixedVars)})"
  }

  /** Every cdgp.Pi.x variable should be bounded by the same range as x variable. */
  def updateRange(range: Seq[VarRange], prefixedVars: Seq[String], id: Int): Seq[VarRange] = {
    range.zip(prefixedVars).flatMap{
      case (r @ Range(v, lb, ub, lbS, ubS, oper), newV) => List(r, Range(newV, lb, ub, lbS, ubS, oper))
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
                        val lbSign: String = ">=", val ubSign: String = "<=", val operand: String = "and") {
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
      val implCond = if (implCondParts.size > 1) implCondParts.mkString(s"($operand ", " ", ")") else implCondParts.head
      implCond
    }
  }
}
case class EmptyRange() extends VarRange("", None, None)
case class Range(override val varName: String,
                 override val lb: Option[Double] = None,
                 override val ub: Option[Double] = None,
                 override val lbSign: String = ">=",
                 override val ubSign: String = "<=",
                 override val operand: String = "and") extends VarRange(varName, lb, ub, lbSign, ubSign, operand)
object Range {
  def apply(varName: String, lb: Double, ub: Double): Range = {
    Range(varName, Some(lb), Some(ub), lbSign=">=", ubSign="<=", operand="and")
  }
  def diffThan(varName: String, d: Double): Range = Range(varName, Some(d), Some(d), lbSign=">", ubSign="<", operand="or")
}



object RegressionConstraints {
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
    b.tests.foreach { case (in, out) =>
      s += s"(constraint (= ($sfName ${Tools.double2str(in).mkString(" ")}) ${Tools.double2str(out)}))\n"
    }
    s
  }


  /**
    * Returns data in the SyGuS competition format. Constraints are included.
    */
  def generateInSygusFormat(b: Benchmark, mergeFormalConstr: Boolean = false, logic: String = "NRA"): String = {
    val sfName = b.funName
    var s = s"(set-logic $logic)\n"
    s += s"(synth-fun $sfName ${b.argsSignature} Real)\n"
    // Synthesis variables
    s += b.vars.map { x => s"(declare-var $x Real)" }.mkString("", "\n", "\n")
    // Some helper variables
    //s += b.vars.map{ x => s"(declare-fun ${x}_2 () Real)" }.mkString("", "\n", "\n")

    s += generateConstrTestCases(b) + "\n"


    val decls = mutable.ListBuffer[String]()
    val constr = mutable.ListBuffer[String]()

    b.props.indices.zip(b.props).foreach { case (i, prop) =>
      val (d, c) = prop.encode(i, b)
      decls ++= d
      constr ++= c
    }

    s += decls.mkString("\n")
    s += "\n\n"
    if (mergeFormalConstr)
      s += constr.mkString("(constraint (and\n    ", "\n    ", "))")
    else
      s += constr.map(c => s"(constraint $c)").mkString("\n")
    s += "\n\n(check-synth)\n"
    s
  }


  /**
    * Returns data in the tab-separated values format, with the first row containing names of the variables.
    * Constraints are ignored.
    */
  def generateInTsvFormat(b: Benchmark): String = {
    val header = b.vars :+ b.funName
    header.mkString("", "\t", "\n") +
      b.tests.map {case (inputs, value) => Tools.double2str(inputs :+ value).mkString("\t")}.mkString("\n")
  }
}

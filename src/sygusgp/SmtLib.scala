package sygusgp

import sygus14.SyGuS14
import sygus14.SyGuS14._
import swim.tree.Op
import sygus.Term
import sygus.BitVecSortExpr
import sygus.ConstraintCmd
import sygus.SortExpr
import sygus.RealSortExpr
import sygus.BoolSortExpr
import sygus.VarDeclCmd
import sygus.IntSortExpr
import sygus16.SyGuS16
import sygus.SynthFunCmd

/* Functions for converting the SMTLIB and Sygus terms into input to 
 * a solver (represented as strings)
 */
object SMTLIBFormatter {
  def sortToString(s: SortExpr): String = s match {
    case IntSortExpr()          => "Int"
    case BoolSortExpr()         => "Bool" 
    case RealSortExpr()         => "Real" 
    case BitVecSortExpr(n: Int) => f"BV$n" // TODO:?
  }

  /* Produces the input to the solver for verifying if program p is correct
   * wrt the specification given by problem.
   */
  def verify(problem: SyGuS16, p: Op): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head // head!
    val args = sf.list.map { case (k, v) => f"($k ${sortToString(v)})" } mkString
    val fv = problem.cmds.collect { case v: VarDeclCmd => v }
    val constraints = problem.cmds.collect {
      case ConstraintCmd(t: Term) => f"${nestedProductToString(t)}"
    } mkString("\n")

    f"(set-logic ${problem.setLogic.get.id})\n" +
      f"(define-fun ${sf.sym} ($args) ${sortToString(sf.se)} ${apply(p)})\n" +
      fv.map(v => f"(declare-var ${v.sym} ${sortToString(v.sortExpr)})").mkString("\n") +
      f"\n(assert (not (and $constraints)))" // 'and' works also for one argument
  }

  /* Query for checking whether the given output produced by a program for a given input
   * is correct wrt the specification given by the problem.
   * This is done by copying most of the problem and defining a constant function 
   * that returns the output value. 
   */
  def checkOnInput(problem: SyGuS16, input: Map[String, Any], output: Any): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head // head!
    val args = sf.list.map { case (k, v) => f"($k ${sortToString(v)})" } mkString
    val fv = problem.cmds.collect { case v: VarDeclCmd => v }
    val constraints = problem.cmds.collect {
      case ConstraintCmd(t: Term) => f"${nestedProductToString(t)}"
    } mkString("\n")
    f"(set-logic ${problem.setLogic.get.id})\n" +
      f"(define-fun ${sf.sym} ($args) ${sortToString(sf.se)} $output)\n" +
      fv.map(v => f"(define-fun ${v.sym} () ${sortToString(v.sortExpr)} ${input(v.sym)})").mkString("\n") +
      f"\n(assert (and $constraints))" // 'and' works also for one argument
  }
  
  /* Query for searching for the output correct wrt the specification and the
   * specified inputs.
   */
  def searchForCorrectOutput(problem: SyGuS16, input: Map[String, Any]): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head // synth-fun
    val args = sf.list.map { case (k, v) => f"($k ${sortToString(v)})" } mkString
    val fv = problem.cmds.collect { case v: VarDeclCmd => v }
    val constraints = problem.cmds.collect {
      case ConstraintCmd(t: Term) => f"${nestedProductToString(t)}"
    } mkString("\n")
    val sfSort = sortToString(sf.se)
    f"(set-logic ${problem.setLogic.get.id})\n" +
      f"(declare-fun CorrectOutput () ${sfSort})\n" +
      f"(define-fun ${sf.sym} ($args) ${sfSort} CorrectOutput)\n" +
      fv.map(v => f"(define-fun ${v.sym} () ${sortToString(v.sortExpr)} ${input(v.sym)})").mkString("\n") +
      f"\n(assert (and $constraints))" // 'and' works also for one argument
  }

  def apply(op: Op): String = op.args.size match {
    case 0 => op.op.toString
    case _ => f"(${op.op}" + op.args.
      map(apply(_)).fold("")(_ + " " + _) + ")"
  }

  def nestedProductToString(p: Any): String = p match {
    case seq: Seq[Any] => seq.map(nestedProductToString(_)).reduce(_ + " " + _)
    case prod: Product => prod.productArity match {
      case 1 => nestedProductToString(prod.productElement(0))
      case _ => "(" + prod.productIterator.
        map(nestedProductToString(_)).reduce(_ + " " + _) + ")"
    }
    case _ => p.toString
  }
}  




/*
  def verifyNaive(problem: SyGuS14, defineFunStr: String = ""): String = {
    val vars = problem.cmds.map {
      case VarDeclCmd(sym: String, sortExpr: SortExpr) =>
        Some(f"($sym ${apply(sortExpr)})")
      case _ => None
    }.flatten.mkString("")
    f"(set-logic ${problem.setLogic.get.id})\n" + defineFunStr + "\n" +
      problem.cmds.map {
        //        case VarDeclCmd(sym: String, sortExpr: SortExpr) =>
        ////          Some(f"(declare-var $sym ${apply(sortExpr)})")
        case ConstraintCmd(t: Term) =>
          ///          Some(f"(assert ${nestedProductToString(t)})")
          Some(f"(assert (forall ($vars) ${nestedProductToString(t)}))")
        case _ => None
      }.flatten.mkString("\n")
  }

  def apply(f: Op, name: String, args: Map[String, SortExpr], retSort: SortExpr) : String = {
    val argsS = args.map { case (k, v) => f"($k ${apply(v)})" }.mkString
    f"(define-fun $name ($argsS) ${apply(retSort)} ${apply(f)})"
  }
    // result variable
    // val r = "resultf8ey3r"
    //        (declare-var $r ${apply(sf.se)})
    // f"(assert (= $r (${sf.sym} ${sf.list.unzip._1.mkString(" ")})"

  * 
  */
 
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
import sygus.StringSortExpr
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
    case StringSortExpr()       => "String"
    case BitVecSortExpr(n: Int) => f"BV$n" // TODO:?
  }

  def opToString(op: Op): String = {
    val opStr = if (op.op.isInstanceOf[Symbol]) op.op.toString.tail else op.op.toString
    if (op.args.isEmpty) opStr
    else f"($opStr ${op.args.map(opToString(_)).mkString(" ")})"
  }

  def synthTaskSolutionToString(sst: SygusSynthesisTask, solution: Op): String = {
    val bestBody = SMTLIBFormatter.opToString(solution)
    val args = SMTLIBFormatter.synthFunArgsToString(sst)
    val tpe = SMTLIBFormatter.sortToString(sst.outputType)
    f"(define-fun ${sst.fname} ($args) $tpe\n\t$bestBody)"
  }

  def synthFunArgsToString(sst: SygusSynthesisTask): String = {
    sst.arguments.map { case (k, v) => f"($k ${sortToString(v)})" }.mkString
  }

  def synthFunArgsToString(sfc: SynthFunCmd): String = {
    sfc.list.map { case (k, v) => f"($k ${sortToString(v)})" }.mkString
  }

  def getLogicName(problem: SyGuS16): String = {
    f"${problem.setLogic.get.id}" match {
      case "SLIA" => "QF_S"
      case s => s
    }
  }

  /* Produces the input to the solver for verifying if program p is correct
   * wrt the specification given by problem.
   */
  def verify(problem: SyGuS16, p: Op, solverTimeout: Int = 0): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head // head!
    val sfArgs = synthFunArgsToString(sf)
    val fv = problem.cmds.collect { case v: VarDeclCmd => v }
    val constraints = problem.cmds.collect {
      case ConstraintCmd(t: Term) => f"${nestedProductToString(t)}"
    }.mkString("\n")
    f"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) f"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      //"(set-option :incremental true)\n" +  // for compatibility with CVC4
      f"(define-fun ${sf.sym} ($sfArgs) ${sortToString(sf.se)} ${apply(p)})\n" +
      fv.map(v => f"(declare-fun ${v.sym} () ${sortToString(v.sortExpr)})").mkString("\n") +
      f"\n(assert (not (and $constraints)))\n" // 'and' works also for one argument
  }

  /* Query for checking whether the given output produced by a program for a given input
   * is correct wrt the specification given by the problem.
   * This is done by copying most of the problem and defining a constant function 
   * that returns the output value. 
   */
  def checkOnInput(problem: SyGuS16, input: Map[String, Any], output: Any, solverTimeout: Int = 0): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head // head!
    val sfArgs = synthFunArgsToString(sf)
    val fv = problem.cmds.collect { case v: VarDeclCmd => v }
    val constraints = problem.cmds.collect {
      case ConstraintCmd(t: Term) => f"${nestedProductToString(t)}"
    }.mkString("\n")
    f"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) f"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      f"(define-fun ${sf.sym} ($sfArgs) ${sortToString(sf.se)} $output)\n" +
      fv.map(v => f"(define-fun ${v.sym} () ${sortToString(v.sortExpr)} ${input(v.sym)})").mkString("\n") +
      f"\n(assert (and $constraints))\n" // 'and' works also for one argument
  }
  
  /* Query for searching for the output correct wrt the specification and the
   * specified inputs.
   */
  def searchForCorrectOutput(problem: SyGuS16, input: Map[String, Any], solverTimeout: Int = 0): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head // synth-fun
    val sfArgs = synthFunArgsToString(sf)
    val fv = problem.cmds.collect { case v: VarDeclCmd => v }
    val constraints = problem.cmds.collect {
      case ConstraintCmd(t: Term) => f"${nestedProductToString(t)}"
    }.mkString("\n")
    val sfSort = sortToString(sf.se)
    f"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) f"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      f"(declare-fun CorrectOutput () $sfSort)\n" +
      f"(define-fun ${sf.sym} ($sfArgs) $sfSort CorrectOutput)\n" +
      fv.map(v => f"(define-fun ${v.sym} () ${sortToString(v.sortExpr)} ${input(v.sym)})").mkString("\n") +
      f"\n(assert (and $constraints))\n" // 'and' works also for one argument
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
 
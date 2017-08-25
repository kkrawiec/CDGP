package cdgp

import sygus14.SyGuS14
import sygus14.SyGuS14._
import swim.tree.Op
import sygus._
import sygus16.SyGuS16

/**
  * Functions for converting the SMTLIB and Sygus terms into input to
  * an SMT solver (represented as strings)
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

  /**
    * Produces the input to the solver for verifying if program p is correct
    * wrt the specification given by problem.
    */
  def verify(problem: SyGuS16, p: Op, solverTimeout: Int = 0): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head // head!
    val sfArgs = synthFunArgsToString(sf)
    val varsDecl = problem.cmds.collect { case v: VarDeclCmd => v }
    val constraints = problem.cmds.collect {
      case ConstraintCmd(t: Term) => f"${nestedProductToString(t)}"
    }.mkString("\n")
    f"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) f"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      //"(set-option :incremental true)\n" +  // for compatibility with CVC4
      f"(define-fun ${sf.sym} ($sfArgs) ${sortToString(sf.se)} ${apply(p)})\n" +
      varsDecl.map(v => f"(declare-fun ${v.sym} () ${sortToString(v.sortExpr)})").mkString("\n") +
      f"\n(assert (not (and $constraints)))\n" // 'and' works also for one argument
  }

  /**
    * Query for checking whether the given output produced by a program for a given
    * input is correct wrt the specification given by the problem.
    * This is done by copying most of the problem and defining a constant function
    * that returns the output value.
    */
  def checkOnInput(problem: SyGuS16, input: Map[String, Any], output: Any, solverTimeout: Int = 0): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head // head!
    val sfArgs = synthFunArgsToString(sf)
    val varsDecl = problem.cmds.collect { case v: VarDeclCmd => v }
    val constraints = problem.cmds.collect {
      case ConstraintCmd(t: Term) => f"${nestedProductToString(t)}"
    }.mkString("\n")
    f"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) f"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      f"(define-fun ${sf.sym} ($sfArgs) ${sortToString(sf.se)} $output)\n" +
      varsDecl.map(v => f"(define-fun ${v.sym} () ${sortToString(v.sortExpr)} ${input(v.sym)})").mkString("\n") +
      f"\n(assert (and $constraints))\n" // 'and' works also for one argument
  }
  
  /**
    * Query for searching for the output correct wrt the specification and the
    *  specified inputs.
    */
  def searchForCorrectOutput(problem: SyGuS16, input: Map[String, Any], solverTimeout: Int = 0): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head // synth-fun
    val sfArgs = synthFunArgsToString(sf)
    val varsDecl = problem.cmds.collect { case v: VarDeclCmd => v }
    val constraints = problem.cmds.collect {
      case ConstraintCmd(t: Term) => f"${nestedProductToString(t)}"
    }.mkString("\n")
    val sfSort = sortToString(sf.se)
    f"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) f"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      f"(declare-fun CorrectOutput () $sfSort)\n" +
      f"(define-fun ${sf.sym} ($sfArgs) $sfSort CorrectOutput)\n" +
      varsDecl.map(v => f"(define-fun ${v.sym} () ${sortToString(v.sortExpr)} ${input(v.sym)})").mkString("\n") +
      f"\n(assert (and $constraints))\n" // 'and' works also for one argument
  }

  /**
    * Query for checking, if for the given problem for any input there is always a single
    * correct output. This is required to be able to use the most efficient test cases
    * mechanism instead of SMT solver to obtain fitness for the GP.
    */
  def checkIfOnlySingleCorrectAnswer(problem: SyGuS16, solverTimeout: Int = 0): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head // synth-fun
    val sfArgs = synthFunArgsToString(sf)
    val varsDecl = problem.cmds.collect {
      case v: VarDeclCmd => f"(declare-fun ${v.sym} () ${sortToString(v.sortExpr)})"
    }.mkString("", "\n", "\n")
    val vMap = Map(sf.sym -> (sf.sym+"__2"))
    val cmds1 = problem.cmds
    val cmds2 = problem.cmds.map(SygusUtils.renameNamesInCmd(_, vMap))
    val body1 = cmds1.collect {
      case ConstraintCmd(t: Term) => f"(assert ${nestedProductToString(t)})"
    }.mkString("", "\n", "\n")
    val body2 = cmds2.collect {
      case ConstraintCmd(t: Term) => f"(assert ${nestedProductToString(t)})"
    }.mkString("", "\n", "\n")

    val synthFunSort = sortToString(sf.se)

    val sfSort = sortToString(sf.se)
    f"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) f"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      //"(set-option :incremental true)\n" +  // for compatibility with CVC4
      f"(declare-fun res1__2 () ${synthFunSort})\n" +
      f"(declare-fun res2__2 () ${synthFunSort})\n" +
      f"(define-fun ${sf.sym} ($sfArgs) ${sortToString(sf.se)} res1__2)\n" +
      f"(define-fun ${sf.sym}__2 ($sfArgs) ${sortToString(sf.se)} res2__2)\n\n" +
      varsDecl + "\n" +
      body1 + "\n" +
      body2 + "\n" +
      f"(assert (distinct res1__2 res2__2))"
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
 
package cdgp

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

  def apply(op: Op): String = opToString(op)

  def normalizeTerminal(x: String): String = {
    if (x.head == '-') s"(- ${x.tail})"  // special treatment for negative numbers
    else x
  }

  def opToString(op: Op): String = {
    val opStr = if (op.op.isInstanceOf[Symbol]) op.op.toString.tail else op.op.toString
    if (op.args.isEmpty) normalizeTerminal(opStr)
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
    *
    * An example of the query:
    * <pre>{@code
    *   (set-logic LIA)
    *   (define-fun max2 ((x Int)(y Int)) Int (ite (>= x y) x 0))
    *   (declare-fun x () Int)
    *   (declare-fun y () Int)
    *   (assert (not (and (>= (max2 x y) x)
    *   (>= (max2 x y) y)
    *   (or (= x (max2 x y)) (= y (max2 x y))))))
    * }</pre>
    * Sat means that there is a counterexample, unsat means perfect program was found.
    */
  def verify(problem: SyGuS16, program: Op, solverTimeout: Int = 0): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head
    val sfArgs = synthFunArgsToString(sf)
    val programBody = opToString(program)
    val varsDecl = problem.cmds.collect { case v: VarDeclCmd => v }
    val constraints = problem.cmds.collect {
      case ConstraintCmd(t: Term) => f"${nestedProductToString(t)}"
    }.mkString("\n")
    val auxiliaries = getCodeForAuxiliaries(problem)
    f"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) f"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      f"(define-fun ${sf.sym} ($sfArgs) ${sortToString(sf.se)} $programBody)\n" +
      varsDecl.map(v => f"(declare-fun ${v.sym} () ${sortToString(v.sortExpr)})").mkString("\n") +
      f"\n(assert (not (and $constraints)))\n" // 'and' works also for one argument
  }


  /**
    * Query for checking whether the given output produced by a program for a given
    * input is correct wrt the specification given by the problem.
    * This is done by copying most of the problem and defining a constant function
    * that returns the output value.
    *
    * An example of the query:
    * <pre>{@code
    *   (set-logic LIA)
    *   (define-fun max2 ((x Int)(y Int)) Int 5)
    *   (define-fun x () Int 5)
    *   (define-fun y () Int 1)
    *   (assert (and (>= (max2 x y) x)
    *   (>= (max2 x y) y)
    *   (or (= x (max2 x y)) (= y (max2 x y)))))
    * }</pre>
    * The result is either sat or unsat, model usually will be empty.
    * Sat means that the answer is correct.
    */
  def checkOnInputAndKnownOutput(problem: SyGuS16, input: Map[String, Any],
                                 output: Any, solverTimeout: Int = 0): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head
    val sfArgs = synthFunArgsToString(sf)
    val varsDecl = problem.cmds.collect { case v: VarDeclCmd => v }
    val constraints = problem.cmds.collect {
      case ConstraintCmd(t: Term) => f"${nestedProductToString(t)}"
    }.mkString("\n")
    val auxiliaries = getCodeForAuxiliaries(problem)
    val textOutput = normalizeTerminal(output.toString)
    f"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) f"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      f"(define-fun ${sf.sym} ($sfArgs) ${sortToString(sf.se)} $textOutput)\n" +
      varsDecl.map(v => f"(define-fun ${v.sym} ()" +
        f" ${sortToString(v.sortExpr)} ${normalizeTerminal(input(v.sym).toString)})").mkString("\n") +
      f"\n(assert (and $constraints))\n" // 'and' works also for one argument
  }


  /**
    * Query for checking whether the given output produced by a program for a given
    * input is correct wrt the specification given by the problem.
    * This is done by copying most of the problem and defining a constant function
    * that returns the output value.
    *
    * An example of the query:
    * <pre>{@code
    *   (set-logic LIA)
    *   (define-fun max2 ((x Int)(y Int)) Int (ite (>= x y) x 0))
    *   (define-fun x () Int 5)
    *   (define-fun y () Int 1)
    *   (assert (and (>= (max2 x y) x)
    *   (>= (max2 x y) y)
    *   (or (= x (max2 x y)) (= y (max2 x y)))))
    * }</pre>
    * The result is either sat or unsat, model usually will be empty.
    * Sat means that the answer is correct.
    */
  def checkOnInput(problem: SyGuS16, input: Map[String, Any],
                   program: Op, solverTimeout: Int = 0): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head
    val sfArgs = synthFunArgsToString(sf)
    val varsDecl = problem.cmds.collect { case v: VarDeclCmd => v }
    val programBody = opToString(program)
    val constraints = problem.cmds.collect {
      case ConstraintCmd(t: Term) => f"${nestedProductToString(t)}"
    }.mkString("\n")
    val auxiliaries = getCodeForAuxiliaries(problem)
    f"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) f"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      f"(define-fun ${sf.sym} ($sfArgs) ${sortToString(sf.se)} $programBody)\n" +
      varsDecl.map(v => f"(define-fun ${v.sym} ()" +
        f" ${sortToString(v.sortExpr)} ${normalizeTerminal(input(v.sym).toString)})").mkString("\n") +
      f"\n(assert (and $constraints))\n"
  }


  /**
    * Query for searching for the output correct wrt the specification and the
    * specified inputs.
    *
    * An example of the query:
    * <pre>{@code
    *   (set-logic LIA)
    *   (declare-fun CorrectOutput () Int)
    *   (define-fun max2 ((x Int)(y Int)) Int CorrectOutput)
    *   (define-fun x () Int 4)
    *   (define-fun y () Int 3)
    *   (assert (and (>= (max2 x y) x)
    *   (>= (max2 x y) y)
    *   (or (= x (max2 x y)) (= y (max2 x y)))))
    * }</pre>
    * Sat means that correct output was found, unsat that there is no output
    * consistent with the specification (this probably means that problem was
    * wrongly specified).
    */
  def findOutputForTestCase(problem: SyGuS16, input: Map[String, Any],
                            solverTimeout: Int = 0): String = {
    val sf = problem.cmds.collect { case sf: SynthFunCmd => sf }.head
    val sfArgs = synthFunArgsToString(sf)
    val varsDecl = problem.cmds.collect { case v: VarDeclCmd => v }
    val constraints = problem.cmds.collect {
      case ConstraintCmd(t: Term) => f"${nestedProductToString(t)}"
    }.mkString("\n")
    val auxiliaries = getCodeForAuxiliaries(problem)
    val sfSort = sortToString(sf.se)
    f"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) f"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      f"(declare-fun CorrectOutput () $sfSort)\n" +
      f"(define-fun ${sf.sym} ($sfArgs) $sfSort CorrectOutput)\n" +
      varsDecl.map(v => f"(define-fun ${v.sym} ()" +
        f" ${sortToString(v.sortExpr)} ${normalizeTerminal(input(v.sym).toString)})").mkString("\n") +
      f"\n(assert (and $constraints))\n"
  }


  /**
    * Query for checking, if for the given problem for any input there is always a single
    * correct output. This is required to be able to use the most efficient test cases
    * mechanism instead of SMT solver to obtain fitness for the GP.
    *
    * An example of the query:
    * <pre>{@code
    *   (set-logic LIA)
    *   (declare-fun res1__2 () Int)
    *   (declare-fun res2__2 () Int)
    *   (define-fun max2 ((x Int)(y Int)) Int res1__2)
    *   (define-fun max2__2 ((x Int)(y Int)) Int res2__2)
    *
    *   (declare-fun x () Int)
    *   (declare-fun y () Int)
    *
    *   (assert (>= (max2 x y) x))
    *   (assert (>= (max2 x y) y))
    *   (assert (or (= x (max2 x y)) (= y (max2 x y))))
    *
    *   (assert (>= (max2__2 x y) x))
    *   (assert (>= (max2__2 x y) y))
    *   (assert (or (= x (max2__2 x y)) (= y (max2__2 x y))))
    *
    *   (assert (distinct res1__2 res2__2))
    * }</pre>
    * Sat means that there is at least one input for which there is more than
    * one correct output.
    */
  def checkIfSingleAnswerForEveryInput(problem: SyGuS16, solverTimeout: Int = 0): String = {
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
    val auxiliaries = getCodeForAuxiliaries(problem)
    
    val synthFunSort = sortToString(sf.se)
    val sfSort = sortToString(sf.se)
    f"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) f"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      f"(declare-fun res1__2 () ${synthFunSort})\n" +
      f"(declare-fun res2__2 () ${synthFunSort})\n" +
      f"(define-fun ${sf.sym} ($sfArgs) ${sortToString(sf.se)} res1__2)\n" +
      f"(define-fun ${sf.sym}__2 ($sfArgs) ${sortToString(sf.se)} res2__2)\n\n" +
      varsDecl + "\n" +
      body1 + "\n" +
      body2 + "\n" +
      f"(assert (distinct res1__2 res2__2))"
  }

  def getCodeForAuxiliaries(problem: SyGuS16): String = {
    problem.cmds.collect {
      case FunDeclCmd(name, sortExprs, sortExpr) =>
        val argsSorts = sortExprs.map(sortToString(_)).mkString(" ")
        val retSort = sortToString(sortExpr)
        f"(declare-fun $name ($argsSorts) $retSort)"
      case FunDefCmd(name, sortExprs, sortExpr, term) =>
        val argsSorts = sortExprs.map{ case (n, s) => s"($n ${sortToString(s)})"}.mkString("")
        val retSort = sortToString(sortExpr)
        val body = nestedProductToString(term)
        f"(define-fun $name ($argsSorts) $retSort $body)"
    }.mkString("\n")
  }

  def nestedProductToString(p: Any): String = p match {
    case seq: Seq[Any] => {
      val s = seq.map(nestedProductToString(_))
      s.reduce(_ + " " + _)
    }
    case prod: Product => prod.productArity match {
      case 1 => nestedProductToString(prod.productElement(0))
      case _ => "(" + prod.productIterator.
        map(nestedProductToString(_)).reduce(_ + " " + _) + ")"
    }
    case _ => p.toString
  }
}
 
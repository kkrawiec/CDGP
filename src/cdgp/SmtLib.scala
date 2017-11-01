package cdgp

import swim.tree.Op
import sygus._
import sygus16.SyGuS16


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
class QueryTemplateVerification(problem: SyGuS16,
                                sygusConstr: SygusBenchmarkConstraints,
                                timeout: Int = 0) extends Function1[Op, String] {
  def createTemplate: String = {
    val constraintsPre = SMTLIBFormatter.getCodeForConstraints(sygusConstr.precond)
    val constraintsPost = SMTLIBFormatter.getCodeForMergedConstraints(sygusConstr.postcond)
    val auxiliaries = SMTLIBFormatter.getCodeForAuxiliaries(problem)
    s"(set-logic ${SMTLIBFormatter.getLogicName(problem)})\n" +
      (if (timeout > 0) s"(set-option :timeout $timeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      "%1$s\n" +  // a place to insert target function definition given the program
      sygusConstr.varDecls.map{v => s"(declare-fun ${v.sym} () ${SMTLIBFormatter.sortToString(v.sortExpr)})"}.mkString("", "\n", "\n") +
      constraintsPre +
      s"\n(assert (not $constraintsPost))\n"
  }
  val template: String = createTemplate

  override def apply(program: Op): String = {
    val programBody = SMTLIBFormatter.opToString(program)
    template.format(sygusConstr.synthTask.getSynthFunCode(programBody))
  }
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
class QueryTemplateInputAndKnownOutput(problem: SyGuS16,
                                       sygusConstr: SygusBenchmarkConstraints,
                                       timeout: Int = 0) extends Function2[Map[String, Any], Any, String] {
  def createTemplate: String = {
    val constraints = SMTLIBFormatter.getCodeForMergedConstraints(problem.cmds)
    val auxiliaries = SMTLIBFormatter.getCodeForAuxiliaries(problem)
    s"(set-logic ${SMTLIBFormatter.getLogicName(problem)})\n" +
      (if (timeout > 0) s"(set-option :timeout $timeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      s"${sygusConstr.synthTask.getSynthFunCode("%1$s")}\n" +
      "%2$s\n" +
      s"\n(assert $constraints)\n"
  }
  val template: String = createTemplate

  def apply(input: Map[String, Any], output: Any): String = {
    val textOutput = SMTLIBFormatter.normalizeTerminal(output.toString)
    val textInputs = sygusConstr.varDecls.map { v =>
      s"(define-fun ${v.sym} () " +
        s"${SMTLIBFormatter.sortToString(v.sortExpr)} ${SMTLIBFormatter.normalizeTerminal(input(v.sym).toString)})"
    }.mkString("\n")
    template.format(textOutput, textInputs)
  }
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
class QueryTemplateInputAndUnknownOutput(problem: SyGuS16,
                                         sygusConstr: SygusBenchmarkConstraints,
                                         timeout: Int = 0) extends Function2[Op, Map[String, Any], String] {
  def createTemplate: String = {
    val constraints = SMTLIBFormatter.getCodeForMergedConstraints(problem.cmds)
    val auxiliaries = SMTLIBFormatter.getCodeForAuxiliaries(problem)
    s"(set-logic ${SMTLIBFormatter.getLogicName(problem)})\n" +
      (if (timeout > 0) s"(set-option :timeout $timeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      s"${sygusConstr.synthTask.getSynthFunCode("%1$s")}\n" +
      "%2$s" +
      s"\n(assert $constraints)\n"
  }
  val template: String = createTemplate

  def apply(program: Op, input: Map[String, Any]): String = {
    val programBody = SMTLIBFormatter.opToString(program)
    val textInputs = sygusConstr.varDecls.map{v =>
      s"(define-fun ${v.sym} () " +
      s"${SMTLIBFormatter.sortToString(v.sortExpr)} ${SMTLIBFormatter.normalizeTerminal(input(v.sym).toString)})"
    }.mkString("\n")
    template.format(programBody, textInputs)
  }
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
class QueryTemplateFindOutput(problem: SyGuS16,
                              sygusConstr: SygusBenchmarkConstraints,
                              timeout: Int = 0) extends Function1[Map[String, Any], String] {
  def createTemplate: String = {
    val constraints = SMTLIBFormatter.getCodeForMergedConstraints(problem.cmds)
    val auxiliaries = SMTLIBFormatter.getCodeForAuxiliaries(problem)
    s"(set-logic ${SMTLIBFormatter.getLogicName(problem)})\n" +
      (if (timeout > 0) s"(set-option :timeout $timeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      s"(declare-fun CorrectOutput () ${SMTLIBFormatter.sortToString(sygusConstr.synthTask.outputType)})\n" +
      s"${sygusConstr.synthTask.getSynthFunCode("CorrectOutput")}\n" +
      "%1$s" +
      s"\n(assert $constraints)\n"
  }
  val template: String = createTemplate

  def apply(input: Map[String, Any]): String = {
    val textInputs = sygusConstr.varDecls.map{v =>
      s"(define-fun ${v.sym} () " +
      s"${SMTLIBFormatter.sortToString(v.sortExpr)} ${SMTLIBFormatter.normalizeTerminal(input(v.sym).toString)})"
    }.mkString("\n")
    template.format(textInputs)
  }
}





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
    case BitVecSortExpr(n: Int) => s"BV$n" // TODO:?
  }

  def apply(op: Op): String = opToString(op)

  def normalizeTerminal(x: String): String = {
    if (x.head == '-') s"(- ${x.tail})"  // special treatment for negative numbers
    else x
  }

  def opToString(op: Op): String = {
    val opStr = if (op.op.isInstanceOf[Symbol]) op.op.toString.tail else op.op.toString
    if (op.args.isEmpty) normalizeTerminal(opStr)
    else s"($opStr ${op.args.map(opToString(_)).mkString(" ")})"
  }

  def synthSolutionToString(sst: SygusSynthesisTask, solution: Op): String = {
    val bestBody = opToString(solution)
    val args = synthFunArgsToString(sst)
    val tpe = sortToString(sst.outputType)
    s"(define-fun ${sst.fname} ($args) $tpe\n\t$bestBody)"
  }

  def synthFunArgsToString(sst: SygusSynthesisTask): String = {
    sst.args.map { case (k, v) => s"($k ${sortToString(v)})" }.mkString
  }

  def synthFunArgsToString(sfc: SynthFunCmd): String = {
    sfc.list.map { case (k, v) => s"($k ${sortToString(v)})" }.mkString
  }

  def synthFunArgsToString(sfc: Seq[(String, SortExpr)]): String = {
    sfc.map { case (k, v) => s"($k ${sortToString(v)})" }.mkString
  }

  def getLogicName(problem: SyGuS16): String = {
    s"${problem.setLogic.get.id}" match {
      case "SLIA" => "ALL"// "QF_S"
      case s => s
    }
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
  def checkIfSingleAnswerForEveryInput(sf: SygusSynthesisTask, problem: SyGuS16,
                                       solverTimeout: Int = 0): String = {
    val sfArgs = synthFunArgsToString(sf)
    val varsDecl = problem.cmds.collect {
      case v: VarDeclCmd => s"(declare-fun ${v.sym} () ${sortToString(v.sortExpr)})"
    }.mkString("", "\n", "\n")
    val vMap = Map(sf.fname -> (sf.fname+"__2"))
    val cmds1 = problem.cmds
    val cmds2 = problem.cmds.map(SygusUtils.renameNamesInCmd(_, vMap))
    val body1 = cmds1.collect {
      case ConstraintCmd(t: Term) => s"(assert ${termToSmtlib(t)})"
    }.mkString("", "\n", "\n")
    val body2 = cmds2.collect {
      case ConstraintCmd(t: Term) => s"(assert ${termToSmtlib(t)})"
    }.mkString("", "\n", "\n")
    val auxiliaries = getCodeForAuxiliaries(problem)

    val synthFunSort = sortToString(sf.outputType)
    s"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) s"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      s"(declare-fun res1__2 () $synthFunSort)\n" +
      s"(declare-fun res2__2 () $synthFunSort)\n" +
      s"(define-fun ${sf.fname} ($sfArgs) $synthFunSort res1__2)\n" +
      s"(define-fun ${sf.fname}__2 ($sfArgs) $synthFunSort res2__2)\n\n" +
      varsDecl + "\n" +
      body1 + "\n" +
      body2 + "\n" +
      s"(assert (distinct res1__2 res2__2))"
  }


  def getCodeForAuxiliaries(problem: SyGuS16): String = {
    problem.cmds.collect {
      case FunDeclCmd(name, sortExprs, sortExpr) =>
        val argsSorts = sortExprs.map(sortToString(_)).mkString(" ")
        val retSort = sortToString(sortExpr)
        s"(declare-fun $name ($argsSorts) $retSort)"
      case FunDefCmd(name, sortExprs, sortExpr, term) =>
        val argsSorts = sortExprs.map{ case (n, s) => s"($n ${sortToString(s)})"}.mkString("")
        val retSort = sortToString(sortExpr)
        val body = termToSmtlib(term)
        s"(define-fun $name ($argsSorts) $retSort $body)"
    }.mkString("\n")
  }

  def getCodeForConstraints(cmds: Seq[Cmd]): String = {
    cmds.collect {
      case ConstraintCmd(t: Term) => s"(assert ${termToSmtlib(t)})"
    }.mkString("\n")
  }

  def getCodeForMergedConstraints(cmds: Seq[Cmd]): String = {
    val constraints = cmds.collect {
      case ConstraintCmd(t: Term) => s"${termToSmtlib(t)}"
    }.mkString("\n  ")
    s"(and $constraints)"
  }

  def termToSmtlib(p: Any): String = p match {
    case seq: Seq[Any] => { // in case seq of terms is provided
      val s = seq.map(termToSmtlib(_))
      s.reduce(_ + " " + _)
    }
    case LetTerm(list, term) =>
      val boundVars = list.map{ case (name, _, t) => s"($name ${termToSmtlib(t)})" }
      s"(let (${boundVars.mkString("")}) ${termToSmtlib(term)})"
    case ExistsTerm(list, term) =>
      val boundVars = list.map{ case (name, sort) => s"($name ${sortToString(sort)})" }
      s"(exists (${boundVars.mkString("")}) ${termToSmtlib(term)})"
    case ForallTerm(list, term) =>
      val boundVars = list.map{ case (name, sort) => s"($name ${sortToString(sort)})" }
      s"(forall (${boundVars.mkString("")}) ${termToSmtlib(term)})"
    case prod: Product => prod.productArity match {
      // Product catches any case class
      case 1 => termToSmtlib(prod.productElement(0))
      case _ => "(" + prod.productIterator.   // iterate over all fields
        map(termToSmtlib(_)).reduce(_ + " " + _) + ")"
    }
    case _ => p.toString
  }
}
 
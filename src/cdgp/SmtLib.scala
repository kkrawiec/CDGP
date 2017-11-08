package cdgp

import swim.tree.Op
import sygus._
import sygus16.SyGuS16



class Query(val code: String, val mainCmd: String = "(check-sat)\n",
            val satCmds: String = "", val unsatCmds: String = "") {
  def getScript: String = {
    code +
      (if (mainCmd != "") mainCmd + "\n" else "") +
      (if (satCmds != "") satCmds + "\n" else "") +
      (if (unsatCmds != "") unsatCmds + "\n" else "")
  }

  override def toString: String = getScript
}


class CheckSatQuery(code: String, satCmds: String, unsatCmds: String = "")
  extends Query(code, "(check-sat)", satCmds, unsatCmds) {}

object CheckSatQuery {
  def apply(code: String, satCmds: String, unsatCmds: String = ""): CheckSatQuery =
    new CheckSatQuery(code, satCmds, unsatCmds)
}


class SimplifyQuery(code: String) extends Query(code, "", "", "") {}

object SimplifyQuery {
  def apply(code: String): SimplifyQuery = new SimplifyQuery(code)
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
  *   (check-sat)
  *   (get-value (x y))
  * }</pre>
  * Sat means that there is a counterexample, unsat means perfect program was found.
  */
class TemplateVerification(problem: SyGuS16,
                           sygusConstr: SygusProblemData,
                           timeout: Int = 0) extends Function1[Op, CheckSatQuery] {
  def createTemplate: String = {
    val constraintsPre = SMTLIBFormatter.getCodeForConstraints(sygusConstr.precond)
    val constraintsPost = SMTLIBFormatter.getCodeForMergedConstraints(sygusConstr.formalConstr)
    val auxiliaries = SMTLIBFormatter.getCodeForAuxiliaries(problem)
    s"(set-logic ${SMTLIBFormatter.getLogicName(problem)})\n" +
      (if (timeout > 0) s"(set-option :timeout $timeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      "%1$s\n" +  // a place to insert target function definition given the program
      sygusConstr.varDecls.map{v => s"(declare-fun ${v.sym} () ${SMTLIBFormatter.sortToString(v.sortExpr)})"}.mkString("", "\n", "\n") +
      constraintsPre + // TODO: Is this correct?
      s"\n(assert (not $constraintsPost))\n"
  }
  val template: String = createTemplate
  val satCmds = s"(get-value (${sygusConstr.varDecls.map(_.sym).mkString(" ")}))\n"

  override def apply(program: Op): CheckSatQuery = {
    val programBody = SMTLIBFormatter.opToString(program)
    val code = template.format(sygusConstr.synthTask.getSynthFunCode(programBody))
    CheckSatQuery(code, satCmds)
  }
}




/**
  * Query for checking whether the given constant output is correct wrt the specification
  * for a given input.
  * This is done by substituting provided constants for input values (var-decls) and
  * synth-fun body (see the example below).
  * Single-invocation property is assumed, because function's
  * output is treated as a constant. Because of this, only formal constraints
  * are used.
  *
  * An example query:
  * <pre>{@code
  *   (set-logic LIA)
  *   (define-fun max2 ((x Int)(y Int)) Int 5)
  *   (define-fun x () Int 5)
  *   (define-fun y () Int 1)
  *   (assert (and (>= (max2 x y) x)
  *   (>= (max2 x y) y)
  *   (or (= x (max2 x y)) (= y (max2 x y)))))
  *   (check-sat)
  * }</pre>
  * The result is either sat or unsat, model usually will be empty.
  * Sat means that the answer is correct.
  */
class TemplateIsOutputCorrectForInput(problem: SyGuS16,
                                      sygusConstr: SygusProblemData,
                                      timeout: Int = 0) extends Function2[Map[String, Any], Any, CheckSatQuery] {
  def createTemplate: String = {
    // Test-cases constraints are ignored
    val preconditions = SMTLIBFormatter.getCodeForMergedConstraints(sygusConstr.precond)
    val constraints = SMTLIBFormatter.getCodeForMergedConstraints(sygusConstr.formalConstr)
    val auxiliaries = SMTLIBFormatter.getCodeForAuxiliaries(problem)
    s"(set-logic ${SMTLIBFormatter.getLogicName(problem)})\n" +
      (if (timeout > 0) s"(set-option :timeout $timeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      s"${sygusConstr.synthTask.getSynthFunCode("%1$s")}\n" +
      "%2$s\n" +
      (if (preconditions.nonEmpty) s"\n(assert $preconditions)\n" else "") +
      s"\n(assert $constraints)\n"
  }
  val template: String = createTemplate

  def apply(input: Map[String, Any], output: Any): CheckSatQuery = {
    val textOutput = SMTLIBFormatter.normalizeTerminal(output.toString)
    val textInputs = sygusConstr.varDecls.map { v =>
      s"(define-fun ${v.sym} () " +
        s"${SMTLIBFormatter.sortToString(v.sortExpr)} ${SMTLIBFormatter.normalizeTerminal(input(v.sym).toString)})"
    }.mkString("\n")
    val code = template.format(textOutput, textInputs)
    CheckSatQuery(code, "")
  }
}




/**
  * Query for checking whether the given output produced by a program for a given
  * input is correct wrt the specification.
  * Test-cases constraints are *not* taken into account, because we only want information
  * for the program's correctness in a single point. This must be carefully handled when
  * program's behavior in the point is defined by test cases.
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
  *   (check-sat)
  * }</pre>
  * The result is either sat or unsat, model usually will be empty.
  * Sat means that the answer is correct.
  */
class TemplateIsProgramCorrectForInput(problem: SyGuS16,
                                       sygusConstr: SygusProblemData,
                                       timeout: Int = 0) extends Function2[Op, Map[String, Any], CheckSatQuery] {
  def createTemplate: String = {
    // Test-cases constraints are ignored
    val preconditions = SMTLIBFormatter.getCodeForMergedConstraints(sygusConstr.precond)
    val constraints = SMTLIBFormatter.getCodeForMergedConstraints(sygusConstr.formalConstr)
    val auxiliaries = SMTLIBFormatter.getCodeForAuxiliaries(problem)
    s"(set-logic ${SMTLIBFormatter.getLogicName(problem)})\n" +
      (if (timeout > 0) s"(set-option :timeout $timeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      s"${sygusConstr.synthTask.getSynthFunCode("%1$s")}\n" +
      "%2$s" +
      (if (preconditions.nonEmpty) s"\n(assert $preconditions)\n" else "") +
      s"\n(assert $constraints)\n"
  }
  val template: String = createTemplate

  def apply(program: Op, input: Map[String, Any]): CheckSatQuery = {
    val programBody = SMTLIBFormatter.opToString(program)
    val textInputs = sygusConstr.varDecls.map{v =>
      s"(define-fun ${v.sym} () " +
      s"${SMTLIBFormatter.sortToString(v.sortExpr)} ${SMTLIBFormatter.normalizeTerminal(input(v.sym).toString)})"
    }.mkString("\n")
    val code = template.format(programBody, textInputs)
    CheckSatQuery(code, "")
  }
}




/**
  * Query for searching for any output correct wrt the specification and the
  * specified inputs.
  *
  * NOTE: Single-invocation and single-answer properties are assumed, because function's
  * output is represented as a constant. Because of this, only formal constraints
  * are used. Any test cases different than the provided input automatically lead
  * to unsat (for the great majority of cases).
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
  *   (check-sat)
  *   (get-value (CorrectOutput))
  * }</pre>
  * Sat means that correct output was found, unsat that there is no output
  * consistent with the specification (this probably means that problem was
  * wrongly specified).
  */
class TemplateFindOutput(problem: SyGuS16,
                         sygusConstr: SygusProblemData,
                         timeout: Int = 0) extends Function1[Map[String, Any], CheckSatQuery] {
  def createTemplate: String = {
    // Test-cases constraints are ignored
    // TODO: something is off with this; either median or united unit test is failing.
    val preconditions = SMTLIBFormatter.getCodeForMergedConstraints(sygusConstr.precond)
    val constraints = SMTLIBFormatter.getCodeForMergedConstraints(sygusConstr.formalConstr)
    val auxiliaries = SMTLIBFormatter.getCodeForAuxiliaries(problem)
    s"(set-logic ${SMTLIBFormatter.getLogicName(problem)})\n" +
      (if (timeout > 0) s"(set-option :timeout $timeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      s"(declare-fun CorrectOutput () ${SMTLIBFormatter.sortToString(sygusConstr.synthTask.outputType)})\n" +
      s"${sygusConstr.synthTask.getSynthFunCode("CorrectOutput")}\n" +
      "%1$s" +
      (if (preconditions.nonEmpty) s"\n(assert $preconditions)\n" else "") +
      s"\n(assert $constraints)\n"
  }
  val template: String = createTemplate
  val satCmds: String = s"(get-value (CorrectOutput))\n"

  def apply(input: Map[String, Any]): CheckSatQuery = {
    val textInputs = sygusConstr.varDecls.map{v =>
      s"(define-fun ${v.sym} () " +
      s"${SMTLIBFormatter.sortToString(v.sortExpr)} ${SMTLIBFormatter.normalizeTerminal(input(v.sym).toString)})"
    }.mkString("\n")
    val code = template.format(textInputs)
    CheckSatQuery(code, satCmds)
  }
}





/**
  * Query to simplify the synthesis function.
  *
  * An example of the query:
  * <pre>{@code
  *   (set-logic LIA)
  *
  * }</pre>
  */
class TemplateSimplify(problem: SyGuS16,
                       sygusConstr: SygusProblemData,
                       timeout: Int = 0) extends Function1[Op, SimplifyQuery] {
  def createTemplate: String = {
    // Auxiliaries are added because they may contain function definitions which are
    // used in the solution.
    val auxiliaries = SMTLIBFormatter.getCodeForAuxiliaries(problem)
    s"(set-logic ${SMTLIBFormatter.getLogicName(problem)})\n" +
      (if (timeout > 0) s"(set-option :timeout $timeout)\n" else "") +
      auxiliaries + "\n" +
      SMTLIBFormatter.produceVarDecls(sygusConstr) +
      "(simplify %1$s\n)\n"
  }
  val template: String = createTemplate

  def apply(op: Op): SimplifyQuery = {
    apply(op.toString)
  }
  def apply(opSmtlib: String): SimplifyQuery = {
    SimplifyQuery(template.format(opSmtlib))
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
    synthSolutionToString(sst, bestBody)
  }

  def synthSolutionToString(sst: SygusSynthesisTask, solutionSmtlib: String): String = {
    val args = synthFunArgsToString(sst)
    val tpe = sortToString(sst.outputType)
    s"(define-fun ${sst.fname} ($args) $tpe\n\t$solutionSmtlib)"
  }

  def produceVarDecls(sygusConstr: SygusProblemData): String = {
    sygusConstr.varDecls.map{v =>
      s"(declare-fun ${v.sym} () ${SMTLIBFormatter.sortToString(v.sortExpr)})"
    }.mkString("", "\n", "\n")
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
    *   (check-sat)
    *   (get-value (x y res1__2 res2__2))
    * }</pre>
    * Sat means that there is at least one input for which there is more than
    * one correct output.
    */
  def checkIfSingleAnswerForEveryInput(sf: SygusSynthesisTask, problem: SyGuS16,
                                       solverTimeout: Int = 0): CheckSatQuery = {
    val sfArgs = synthFunArgsToString(sf)
    val varDecls = problem.cmds.collect { case v: VarDeclCmd => v }
    val varsDeclFunDefs = varDecls.map {
      v: VarDeclCmd => s"(declare-fun ${v.sym} () ${sortToString(v.sortExpr)})"
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
    val code = s"(set-logic ${getLogicName(problem)})\n" +
      (if (solverTimeout > 0) s"(set-option :timeout $solverTimeout)\n" else "") +
      "(set-option :produce-models true)\n" +
      auxiliaries + "\n" +
      s"(declare-fun res1__2 () $synthFunSort)\n" +
      s"(declare-fun res2__2 () $synthFunSort)\n" +
      s"(define-fun ${sf.fname} ($sfArgs) $synthFunSort res1__2)\n" +
      s"(define-fun ${sf.fname}__2 ($sfArgs) $synthFunSort res2__2)\n\n" +
      varsDeclFunDefs + "\n" +
      body1 + "\n" +
      body2 + "\n" +
      s"(assert (distinct res1__2 res2__2))"
    val satCmds = s"(get-value (${varDecls.map(_.sym).mkString(" ")} res1__2 res2__2))\n"
    CheckSatQuery(code, satCmds)
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
    }
    if (constraints.nonEmpty)
      s"(and ${constraints.mkString("\n  ")})"
    else
      ""
  }

  def termToSmtlib(p: Any): String = p match {
    case seq: Seq[Any] => // in case seq of terms is provided
      val s = seq.map(termToSmtlib(_))
      s.reduce(_ + " " + _)
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

  /**
    * Constructs Op given it's string encoding in the form: Op(ARG1, ARG2, ...).
    * As nonterminal symbol assigned will be 'default.
    * For example from "+(-(a, b), c)" will be created Op('+, Op('-, Op('a), Op('b)), Op('c)).
    *
    * @param s string encoding of op.
    * @param delim delimiter which separates arguments of functions (default: " ").
    * @param convertConsts if set to true (default), terminals detected as Boolean, Int, Double or
    * String constants will be converted to instances of those types.
    */
  def smtlibToOp(s: String, delim: String = "\\s+", convertConsts: Boolean = true): Op = {
    def isBoolean(s: String): Boolean = if (s == "true" || s == "false") true else false
    def isInt(s: String): Boolean = try { val x = s.toInt; true } catch { case _:Throwable => false }
    def isDouble(s: String): Boolean = try { val x = s.toDouble; true } catch { case _:Throwable => false }
    def isString(s: String): Boolean = if (s.head == '\"' && s.last == '\"') true else false
    def getTerminalOp(s: String): Any = {
      if (convertConsts)
        if (isBoolean(s)) s.toBoolean
        else if (isInt(s)) s.toInt
        else if (isDouble(s)) s.toDouble
        else if (isString(s)) s.substring(1, s.size-1)
        else Symbol(s)
      else
        Symbol(s)
    }
    def getNt(symb: Symbol): Symbol = 'default
    def getNtForTerminal(value: Any): Symbol = 'default
    def getMatchingParenthIndex(words: Array[String], begin: Int): Int = {
      var parOpened = 1
      for (i <- (begin+1) until words.size) {
        if (words(i) == ")") parOpened -= 1
        else if (words(i) == "(") parOpened += 1
        if (parOpened == 0)
          return i
      }
      words.size
    }
    def getArgs(words: Array[String]): List[Op] = {
      var i = 0
      var args = List[Op]()
      while (i < words.size) {
        if (words(i) != "(") {
          val value = getTerminalOp(words(i))
          val nt = getNtForTerminal(value)
          args = args :+ Op(nt, value)
          i += 1
        }
        else {
          val matchParIndex = getMatchingParenthIndex(words, i)
          val text = words.slice(i, matchParIndex+1).mkString(" ")
          args = args :+ smtlibToOp(text, delim, convertConsts)
          i = matchParIndex + 1
        }
      }
      args
    }
    try {
      val words = s.replace("(", " ( ").replace(")", " ) ").split(delim).filter(!_.isEmpty())
      if (words.head != "(") {
        val value = getTerminalOp(words.head)
        val nt = getNtForTerminal(value)
        Op(nt, value) // Returning terminal.
      }
      else {
        val op = words(1)
        val args = getArgs(words.slice(2, words.size-1))
        Op(getNt(Symbol(op)), Symbol(op), args:_*)
      }
    } catch {
      case _:Throwable => throw new Exception(s"Wrong encoding of Op instance: $s!")
    }
  }
}
 
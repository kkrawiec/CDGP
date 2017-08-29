package tests

import cdgp.{CDGPState, GetValueParser, LoadSygusBenchmark, SMTLIBFormatter}
import fuel.util.{CollectorStdout, Options, Rng}
import org.junit.Test
import org.junit.Assert._
import swim.tree.Op


object TestCDGPState {
  val scriptMax =
"""(set-logic LIA)
(synth-fun max2 ((x Int) (y Int)) Int
  ((Start Int (x y 0 1
(+ Start Start)
(- Start Start)
(ite StartBool Start Start)))
(StartBool Bool ((and StartBool StartBool)
  (or StartBool StartBool)
  (not StartBool)
  (<= Start Start)
  (= Start Start)
  (>= Start Start)))))
(declare-var x Int)
(declare-var y Int)
(constraint (>= (max2 x y) x))
(constraint (>= (max2 x y) y))
(constraint (or (= x (max2 x y)) (= y (max2 x y))))
(check-synth)"""

  val scriptMaxRenamedVars =
"""(set-logic LIA)
(synth-fun max2 ((a Int) (b Int)) Int
  ((Start Int (a b 0 1
(+ Start Start)
(- Start Start)
(ite StartBool Start Start)))
(StartBool Bool ((and StartBool StartBool)
  (or StartBool StartBool)
  (not StartBool)
  (<= Start Start)
  (= Start Start)
  (>= Start Start)))))
(declare-var x Int)
(declare-var y Int)
(constraint (>= (max2 x y) x))
(constraint (>= (max2 x y) y))
(constraint (or (= x (max2 x y)) (= y (max2 x y))))
(check-synth)"""

  val scriptPsuedoMaxRenamedVars =
"""(set-logic LIA)
(synth-fun max2 ((a Int) (b Int)) Int
  ((Start Int (a b 0 1
(+ Start Start)
(- Start Start)
(ite StartBool Start Start)))
(StartBool Bool ((and StartBool StartBool)
  (or StartBool StartBool)
  (not StartBool)
  (<= Start Start)
  (= Start Start)
  (>= Start Start)))))
(declare-var x Int)
(declare-var y Int)
(constraint (>= (max2 x y) x))
(constraint (>= (max2 x y) y))
(check-synth)"""

  val scriptMaxFixedX =
    """(set-logic LIA)
(synth-fun max2 ((x Int) (y Int)) Int
  ((Start Int (x y 0 1
(+ Start Start)
(- Start Start)
(ite StartBool Start Start)))
(StartBool Bool ((and StartBool StartBool)
  (or StartBool StartBool)
  (not StartBool)
  (<= Start Start)
  (= Start Start)
  (>= Start Start)))))
(declare-var y Int)
(constraint (>= (max2 1 y) 1))
(constraint (>= (max2 1 y) y))
(constraint (or (= 1 (max2 1 y)) (= y (max2 1 y))))
(check-synth)"""

  val scriptNotSingleInvocation =
"""; three.sl
; Synthesize x * 3 mod 10
(set-logic LIA)
(synth-fun f ((x Int)) Int
   ((Start Int (x 3 7 10 (* Start Start) (mod Start Start)))))
(declare-var x Int)
(constraint (= (f x) (f (+ x 10))))
(constraint (= (f 1) 3))
(constraint (= (f 2) 6))
(constraint (= (f 3) 9))
(constraint (= (f 4) 2))
(constraint (= (f 5) 5))
(constraint (= (f 6) 8))
(constraint (= (f 7) 1))
(constraint (= (f 8) 4))
(constraint (= (f 9) 7))
(constraint (= (f 0) 0))
(check-synth)
"""
}

final class TestCDGPState {
  implicit val emptyOpt = Options("--searchAlgorithm Lexicase --solverPath " +
    f"${Global.solverPath}")
  implicit val coll = CollectorStdout(emptyOpt)
  implicit val rng = Rng(emptyOpt)

  @Test
  def test_evalOnTestsMax(): Unit = {
    val code = TestCDGPState.scriptMax
    val problem = LoadSygusBenchmark.parseText(code)
    val state = new CDGPState(problem)
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=false)
    val t1 = (GetValueParser("((x 4)(y 3))").toMap, Some(4))
    val t2 = (GetValueParser("((x 5)(y 1))").toMap, Some(5))
    val t3 = (GetValueParser("((x 1)(y 3))").toMap, Some(3))
    val tests = Seq(t1, t2, t3)
    assertEquals(Seq(0, 0, 1), state.evalOnTests(op, tests))
  }

  @Test
  def test_evalOnTestsMaxUsingSolver(): Unit = {
    val code = TestCDGPState.scriptMax
    val problem = LoadSygusBenchmark.parseText(code)
    val state = new CDGPState(problem)
    assertEquals(true, state.useDomainToComputeFitness)
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=false)
    val t1 = (GetValueParser("((x 4)(y 3))").toMap, None)
    val t2 = (GetValueParser("((x 5)(y 1))").toMap, None)
    val t3 = (GetValueParser("((x 1)(y 3))").toMap, None)
    state.testsManager.addNewTest(t1)
    state.testsManager.addNewTest(t2)
    state.testsManager.addNewTest(t3)
    state.testsManager.flushHelpers()
    assertEquals(3, state.testsManager.getNumberOfTests)
    assertEquals(0, state.testsManager.getNumberOfKnownOutputs)
    assertEquals(Seq(0, 0, 1), state.evalOnTests(op, state.testsManager.getTests()))
    assertEquals(2, state.testsManager.getNumberOfKnownOutputs)
    assertEquals(Some(4), state.testsManager.tests(t1._1))
    assertEquals(Some(5), state.testsManager.tests(t2._1))
    assertEquals(None,state.testsManager.tests(t3._1))
  }

  @Test
  def test_evalOnTestsMaxVerify(): Unit = {
    val code = TestCDGPState.scriptMax
    val problem = LoadSygusBenchmark.parseText(code)
    val state = new CDGPState(problem)
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=false)
    val (dec, output) = state.verify(op)
    assertEquals("sat", dec)
    assertEquals(true, output.isDefined)
  }

  @Test
  def test_evalOnTestsMaxDifferentVarOrderInModel(): Unit = {
    val code = TestCDGPState.scriptMax
    val problem = LoadSygusBenchmark.parseText(code)
    val state = new CDGPState(problem)
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=false)
    val t1 = (GetValueParser("((y 3)(x 4))").toMap, Some(4))
    val t2 = (GetValueParser("((y 1)(x 5))").toMap, Some(5))
    val t3 = (GetValueParser("((y 3)(x 1))").toMap, Some(3))
    val tests = Seq(t1, t2, t3)
    assertEquals(Seq(0, 0, 1), state.evalOnTests(op, tests))
  }

  @Test
  def test_evalOnTestsMaxRenamedVars(): Unit = {
    val code = TestCDGPState.scriptMaxRenamedVars
    val problem = LoadSygusBenchmark.parseText(code)
    val state = new CDGPState(problem)
    val op = Op.fromStr("ite(>=(a b) a 0)", useSymbols=false)
    val t1 = (GetValueParser("((x 4)(y 3))").toMap, Some(4))
    val t2 = (GetValueParser("((x 5)(y 1))").toMap, Some(5))
    val t3 = (GetValueParser("((x 1)(y 3))").toMap, Some(3))
    val tests = Seq(t1, t2, t3)
    assertEquals(Seq(0, 0, 1), state.evalOnTests(op, tests))

    val t2_2 = (GetValueParser("((y 1)(x 5))").toMap, Some(5))
    val tests_2 = Seq(t1, t2_2, t3)
    assertEquals(Seq(0, 0, 1), state.evalOnTests(op, tests))
  }

  @Test
  def test_evalOnTestsMaxFixedX(): Unit = {
    val code = TestCDGPState.scriptMaxFixedX
    val problem = LoadSygusBenchmark.parseText(code)
    val state = new CDGPState(problem)
    val op = Op.fromStr("ite(>=(a b) a 0)", useSymbols=false)
    val t1 = (GetValueParser("((asd 4)(y -3))").toMap, Some(1))
    val t2 = (GetValueParser("((asd 5)(y 0))").toMap, Some(1))
    val t3 = (GetValueParser("((asd 1)(y 3))").toMap, Some(3))
    val tests = Seq(t1, t2, t3)
    assertEquals(Seq(0, 0, 1), state.evalOnTests(op, tests))
  }

  @Test
  def test_checkIfOnlySingleCorrectAnswer_unsat(): Unit = {
    val code = TestCDGPState.scriptMaxRenamedVars
    val problem = LoadSygusBenchmark.parseText(code)
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(problem)
    println("query:\n" + query)
    val state = new CDGPState(problem)
    val (decision, output) = state.solver.runSolver(query)
    assertEquals("unsat", decision)  // unsat, so there is only a single answer
    assertEquals(None, output)
  }

  @Test
  def test_checkIfOnlySingleCorrectAnswer_sat(): Unit = {
    val code = TestCDGPState.scriptPsuedoMaxRenamedVars
    val problem = LoadSygusBenchmark.parseText(code)
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(problem)
    println("query:\n" + query)
    val state = new CDGPState(problem)
    val (decision, output) = state.solver.runSolver(query)
    assertEquals("sat", decision)
  }
}
package tests

import org.junit.Test
import org.junit.Assert._
import cdgp._
import fuel.util.{CollectorStdout, Options, Rng}
import swim.tree.Op

final class TestPartialConstraints {
  implicit val emptyOpt = Options(s"--printQueries true --selection lexicase --evolutionMode generational ${Global.solverConfig}")
  implicit val coll = CollectorStdout(emptyOpt)
  implicit val rng = Rng(emptyOpt)

  val sygusMonotonicityOld =
    """
      |(set-logic QF_NRA)
      |(synth-fun f ((x Real)) Real)
      |(declare-var x Real)
      |(constraint (= (f 1.0) 1.0))
      |
      |(declare-fun cdgp.P1.x () Real)
      |(constraint (=> (> cdgp.P1.x x)  (>= (f cdgp.P1.x) (f x))))
      |(check-synth)
    """.stripMargin

  val sygusMonotonicityNew =
    """
      |(set-logic NRA)
      |(synth-fun f ((x Real)) Real)
      |(declare-var x Real)
      |(constraint (= (f 1.0) 1.0))
      |
      |(constraint (forall ((x Real)(cdgp.P1.x Real))
      |   (=> (> cdgp.P1.x x)  (>= (f cdgp.P1.x) (f x)))))
      |(check-synth)
    """.stripMargin

  @Test
  def test_monotonicityOld(): Unit = {
    val problem = LoadSygusBenchmark.parseText(sygusMonotonicityOld)
    val sygusData = SygusProblemData(problem)
    val state = StateCDGP(sygusData)
    val eval = new EvalCDGPSeqDouble(state)

    var op = Op('x)
    var v = eval.getPartialConstraintsVector(op, passValue = 0, nonpassValue = 1)
    println(s"PC Vector for $op: $v")
    assert(0.0 == v.head)

    op = Op('*, Op('x), Op('x))
    v = eval.getPartialConstraintsVector(op, passValue = 0, nonpassValue = 1)
    println(s"PC Vector for $op: $v")
    assert(1.0 == v.head)
  }

  @Test
  def test_monotonicityNew(): Unit = {
    val problem = LoadSygusBenchmark.parseText(sygusMonotonicityNew)
    val sygusData = SygusProblemData(problem)
    val state = StateCDGP(sygusData)
    val eval = new EvalCDGPSeqDouble(state)

    var op = Op('x)
    var v = eval.getPartialConstraintsVector(op, passValue = 0, nonpassValue = 1)
    println(s"PC Vector for $op: $v")
    assert(0.0 == v.head)

    op = Op('*, Op('x), Op('x))
    v = eval.getPartialConstraintsVector(op, passValue = 0, nonpassValue = 1)
    println(s"PC Vector for $op: $v")
    assert(1.0 == v.head)
  }

  @Test
  def test_monotonicityOld_tests(): Unit = {
    val problem = LoadSygusBenchmark.parseText(sygusMonotonicityOld)
    val sygusData = SygusProblemData(problem)
    val state = StateCDGP(sygusData)

    val (_, model) = state.verifyAndParseModel(Op('*, Op('x), Op('x)))
    assertEquals(true, model.isDefined)
    val test = state.createTestFromCounterex(model.get)
    print(s"test:\n$test")
    // Incomplete test. Monotonicity constraint includes a synth-function's call with auxiliary
    // variable, and hence the problem does not have single-invocation property, and all tests
    // are incomplete.
    assertEquals(None, test._2)
    val res1 = state.checkIsProgramCorrectForInput(Op('*, Op('x), Op('x)), Map("x"-> -2.0))
    val res2 = state.checkIsProgramCorrectForInput(Op('x), Map("x"-> -2.0))
    val res3 = state.checkIsProgramCorrectForInput(Op('*, Op('x), Op(2.0)), Map("x"-> -2.0))
    val res4 = state.checkIsProgramCorrectForInput(Op('*, Op('x), Op(-1.0)), Map("x"-> -2.0))
    assertEquals("sat", res1._1)  // this result is not desired
    assertEquals("sat", res2._1)
    assertEquals("sat", res3._1)
    assertEquals("sat", res4._1)  // this result is not desired
  }

  @Test
  def test_monotonicityNew_tests(): Unit = {
    val problem = LoadSygusBenchmark.parseText(sygusMonotonicityNew)
    val sygusData = SygusProblemData(problem)
    val state = StateCDGP(sygusData)

    val (_, model) = state.verifyAndParseModel(Op('*, Op('x), Op('x)))
    assertEquals(true, model.isDefined)
    val test = state.createTestFromCounterex(model.get)
    print(s"test:\n$test")
    // Incomplete test. Monotonicity constraint includes a synth-function's call with auxiliary
    // variable, and hence the problem does not have single-invocation property, and all tests
    // are incomplete.
    assertEquals(None, test._2)
    val res1 = state.checkIsProgramCorrectForInput(Op('*, Op('x), Op('x)), Map("x"-> -2.0))
    val res2 = state.checkIsProgramCorrectForInput(Op('x), Map("x"-> -2.0))
    val res3 = state.checkIsProgramCorrectForInput(Op('*, Op('x), Op(2.0)), Map("x"-> -2.0))
    val res4 = state.checkIsProgramCorrectForInput(Op('*, Op('x), Op(-1.0)), Map("x"-> -2.0))
    assertEquals("unsat", res1._1)
    assertEquals("sat", res2._1)
    assertEquals("sat", res3._1)
    assertEquals("unsat", res4._1)
  }
}

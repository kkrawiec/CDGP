package tests

import cdgp.{CDGPState, LoadSygusBenchmark}
import fuel.util.{CollectorStdout, Options, Rng}
import org.junit.Test
import org.junit.Assert._
import swim.tree.Op


object TestCDGPState {
  val scriptMax =
""" (set-logic LIA)
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

  val scriptMaxDifferentVarOrder =
    """ (set-logic LIA)
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
(declare-var x Int)
(constraint (>= (max2 x y) x))
(constraint (>= (max2 x y) y))
(constraint (or (= x (max2 x y)) (= y (max2 x y))))
(check-synth)"""
}

final class TestCDGPState {
  implicit val emptyOpt = Options("--searchAlgorithm Lexicase")
  implicit val coll = CollectorStdout(emptyOpt)
  implicit val rng = Rng(emptyOpt)

  @Test
  def testEvalOnTestsMax(): Unit = {
    val code = TestCDGPState.scriptMax
    val problem = LoadSygusBenchmark.parseText(code)
    val state = new CDGPState(problem)
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=false)
    val tests = Seq(
      (Map("x"->4, "y"->3), Some(4)),
      (Map("x"->5, "y"->1), Some(5)),
      (Map("x"->1, "y"->3), Some(3)))
    val res = state.evalOnTests(op, tests)
    assertEquals(Seq(0, 0, 1), res)
  }

  @Test
  def testEvalOnTestsMaxDifferentVarOrder(): Unit = {
    val code = TestCDGPState.scriptMaxDifferentVarOrder
    val problem = LoadSygusBenchmark.parseText(code)
    val state = new CDGPState(problem)
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=false)
    val tests = Seq(
      (Map("x"->4, "y"->3), Some(4)),
      (Map("x"->5, "y"->1), Some(5)),
      (Map("x"->1, "y"->3), Some(3)))
    val res = state.evalOnTests(op, tests)
    assertEquals(Seq(1, 1, 1), res)
  }
}
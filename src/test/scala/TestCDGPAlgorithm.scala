package tests

import cdgp._
import fuel.core.StatePop
import fuel.func.TournamentSelection
import fuel.util.{CollectorStdout, Options, Rng}
import org.junit.Test
import org.junit.Assert._
import swim.tree.Op


object TestCDGPAlgorithm {
  val scriptTestsWithTheSameInputs =
"""(set-logic QF_NRA)
(synth-fun waveFn ((x1 Real)) Real)
(declare-var x1 Real)
(constraint (= (waveFn 2.0) 0.1))
(constraint (= (waveFn 1.0) 0.2))
(constraint (= (waveFn 0.0) 0.3))
(constraint (= (waveFn 0.0) 0.4))
(constraint (=> (> x1 0.01) (<= (waveFn x1) 0.0)))
(constraint (=> (< x1 -0.01) (>= (waveFn x1) 0.0)))
(constraint (= (waveFn x1) (- (waveFn (- x1)))))
"""
}



final class TestCDGPAlgorithm {
  val ordFInt = FIntOrdering
  val ordFSeqInt = FSeqIntOrdering
  implicit val rng = Rng(Options("--seed 0"))

  @Test
  def test_FInt(): Unit = {
    val t0_20 = FInt(true, 0, 20, 1)
    val t0_50 = FInt(true, 0, 50, 1)
    val t0_90 = FInt(true, 0, 90, 1)
    val t1_20 = FInt(true, 1, 20, 1)
    val t1_50 = FInt(true, 1, 50, 1)
    val t1_90 = FInt(true, 1, 90, 1)
    val f0_20 = FInt(false, 0, 20, 1)
    val f0_50 = FInt(false, 0, 50, 1)
    val f0_90 = FInt(false, 0, 90, 1)
    val f1_20 = FInt(false, 1, 20, 1)
    val f1_50 = FInt(false, 1, 50, 1)
    val f1_90 = FInt(false, 1, 90, 1)
    assertEquals(1,  ordFInt.compare(t0_50, t0_20))
    assertEquals(0,  ordFInt.compare(t0_50, t0_50))
    assertEquals(-1, ordFInt.compare(t0_50, t0_90))
    assertEquals(1,  ordFInt.compare(t1_50, t0_50))
    assertEquals(-1, ordFInt.compare(t0_50, t1_50))
    assertEquals(-1, ordFInt.compare(t1_20, t1_50))
    assertEquals(0,  ordFInt.compare(t1_50, t1_50))
    assertEquals(1,  ordFInt.compare(t1_90, t1_50))

    assertEquals(-1, ordFInt.compare(t0_50, f0_20))
    assertEquals(-1, ordFInt.compare(t1_50, f0_50))
    assertEquals(1,  ordFInt.compare(f1_50, t1_50))
    assertEquals(1,  ordFInt.compare(f1_90, t1_50))
    assertEquals(-1, ordFInt.compare(t1_90, f0_90))

    assertEquals(1,  ordFInt.compare(f0_50, f0_20))
    assertEquals(0,  ordFInt.compare(f0_50, f0_50))
    assertEquals(-1, ordFInt.compare(f0_50, f0_90))
    assertEquals(1,  ordFInt.compare(f1_50, f0_50))
    assertEquals(-1, ordFInt.compare(f0_20, f1_50))
    assertEquals(-1, ordFInt.compare(f1_20, f1_50))
    assertEquals(0,  ordFInt.compare(f1_50, f1_50))
    assertEquals(1,  ordFInt.compare(f1_90, f1_50))
  }

  @Test
  def test_FInt_Tournament(): Unit = {
    // No assertions in this test, because TournamentSelection is with replacement
    // and it is possible that the same element is chosen several times.
    // This serves rather as a sanity test and code example.
    def s(fit: Int, size: Int) = (Op('nt, "a"), FInt(false, fit, size, 1))
    def r() = (Op('nt, "a"), FInt(false, 1+rng.nextInt(2), 21+rng.nextInt(40), 1))
    def generatePop() = StatePop(List(r(), r(), r(), r(), s(1, 20)))
    1.to(20).foreach { _ =>
      val pop = generatePop()
      val handMin = pop.map(_._2.progSize).min(Ordering[Int])
      println(pop)
      println("sortBy: " + pop.sortBy(_._2)(ordFInt))
      println("minBy: " + pop.minBy(_._2)(ordFInt))
      val x = new TournamentSelection(ordFInt, 5)(rng)(pop)
      println("Selected: " + x)
    }
  }

  @Test
  def test_FSeqInt(): Unit = {
    val t0_20 = FSeqInt(true, Seq(0), 20)
    val t0_50 = FSeqInt(true, Seq(0), 50)
    val t0_90 = FSeqInt(true, Seq(0), 90)
    val t1_20 = FSeqInt(true, Seq(1), 20)
    val t1_50 = FSeqInt(true, Seq(1), 50)
    val t1_90 = FSeqInt(true, Seq(1), 90)
    val f0_20 = FSeqInt(false, Seq(0), 20)
    val f0_50 = FSeqInt(false, Seq(0), 50)
    val f0_90 = FSeqInt(false, Seq(0), 90)
    val f1_20 = FSeqInt(false, Seq(1), 20)
    val f1_50 = FSeqInt(false, Seq(1), 50)
    val f1_90 = FSeqInt(false, Seq(1), 90)
    val longer = FSeqInt(false, Seq(0, 0), 90)
    assertEquals(1,  ordFSeqInt.compare(t0_50, t0_20))
    assertEquals(0,  ordFSeqInt.compare(t0_50, t0_50))
    assertEquals(-1, ordFSeqInt.compare(t0_50, t0_90))
    assertEquals(1,  ordFSeqInt.compare(t1_50, t0_50))
    assertEquals(-1, ordFSeqInt.compare(t0_50, t1_50))
    assertEquals(-1, ordFSeqInt.compare(t1_20, t1_50))
    assertEquals(0,  ordFSeqInt.compare(t1_50, t1_50))
    assertEquals(1,  ordFSeqInt.compare(t1_90, t1_50))

    assertEquals(-1, ordFSeqInt.compare(t0_50, f0_20))
    assertEquals(-1, ordFSeqInt.compare(t1_50, f0_50))
    assertEquals(1,  ordFSeqInt.compare(f1_50, t1_50))
    assertEquals(1,  ordFSeqInt.compare(f1_90, t1_50))
    assertEquals(-1, ordFSeqInt.compare(t1_90, f0_90))

    assertEquals(1,  ordFSeqInt.compare(f0_50, f0_20))
    assertEquals(0,  ordFSeqInt.compare(f0_50, f0_50))
    assertEquals(-1, ordFSeqInt.compare(f0_50, f0_90))
    assertEquals(1,  ordFSeqInt.compare(f1_50, f0_50))
    assertEquals(-1, ordFSeqInt.compare(f0_20, f1_50))
    assertEquals(-1, ordFSeqInt.compare(f1_20, f1_50))
    assertEquals(0,  ordFSeqInt.compare(f1_50, f1_50))
    assertEquals(1,  ordFSeqInt.compare(f1_90, f1_50))

    assertEquals(1,  ordFSeqInt.compare(f1_90, longer))
    assertEquals(-1,  ordFSeqInt.compare(longer, f0_20))
    assertEquals(0,  ordFSeqInt.compare(longer, longer))
  }

  @Test
  def test_FSeqDoubleOrderingMSE(): Unit = {
    val ord = FSeqDoubleOrderingMSE
    val a = FSeqDouble(true, Seq(0.0, 0.0), 20, numPCtests=0)
    val b = FSeqDouble(true, Seq(0.0, 1.0), 20, numPCtests=0)
    val c = FSeqDouble(true, Seq(0.0, 2.0), 20, numPCtests=0)
    val d = FSeqDouble(true, Seq(2.0, 0.0), 20, numPCtests=0)

    assertEquals(-1, ord.compare(a, b))
    assertEquals(-1, ord.compare(a, c))
    assertEquals(1, ord.compare(c, b))
    assertEquals(0, ord.compare(c, d))
  }


  @Test
  def test_testsWithTheSameInputs(): Unit = {
    implicit val emptyOpt = Options(s"--printAddedTests true --selection lexicase --evolutionMode steadyState ${Global.solverConfig}")
    implicit val coll = CollectorStdout(emptyOpt)
    implicit val rng = Rng(emptyOpt)
    val problem = LoadSygusBenchmark.parseText(TestCDGPAlgorithm.scriptTestsWithTheSameInputs)
    val state = StateCDGP(problem)
    val eval = EvalContinuous.EvalCDGPSeqDouble(state, Set("c", "i"))
    val alg = CDGPSteadyStateEpsLexicase(eval)

    assertEquals(4, state.sygusData.testCasesConstrToTests.size)
    assertEquals(4, state.testsManager.newTests.size)
    assertEquals(0, state.testsManager.tests.size)  //it is 0 because tests are flushed after initialization

    val pop = alg.initialize.apply()

    assertEquals(0, state.testsManager.newTests.size)
    assertEquals(4, state.testsManager.tests.size)

    state.testsManager.addNewTest((Map("x1"->2.0), Some(6.0)), allowInputDuplicates=false)
    assertEquals(0, state.testsManager.newTests.size)
    state.testsManager.addNewTest((Map("x1"->2.0), Some(6.0)), allowInputDuplicates=true, allowTestDuplicates=true)
    state.testsManager.addNewTest((Map("x1"->2.0), Some(6.0)), allowInputDuplicates=true, allowTestDuplicates=true)
    assertEquals(2, state.testsManager.newTests.size)
    state.testsManager.flushHelpers()

    assertEquals(0, state.testsManager.newTests.size)
    assertEquals(6, state.testsManager.tests.size)

    state.testsManager.addNewTest((Map("x1"->2.0), Some(6.0)), allowInputDuplicates=true, allowTestDuplicates=false)
    state.testsManager.addNewTest((Map("x1"->2.0), Some(6.0)), allowInputDuplicates=true, allowTestDuplicates=false)
    assertEquals(0, state.testsManager.newTests.size)
    state.testsManager.addNewTest((Map("x1"->2.0), Some(6.0)), allowInputDuplicates=false, allowTestDuplicates=false)
    state.testsManager.addNewTest((Map("x1"->2.0), Some(6.0)), allowInputDuplicates=false, allowTestDuplicates=false)
    assertEquals(0, state.testsManager.newTests.size)
    state.testsManager.flushHelpers()

    assertEquals(0, state.testsManager.newTests.size)
    assertEquals(6, state.testsManager.tests.size)

    state.testsManager.addNewTest((Map("x1"->10.0), Some(6.0)), allowInputDuplicates=true, allowTestDuplicates=false)
    state.testsManager.addNewTest((Map("x1"->10.0), Some(6.0)), allowInputDuplicates=true, allowTestDuplicates=false)
    assertEquals(1, state.testsManager.newTests.size)
    state.testsManager.flushHelpers()

    assertEquals(0, state.testsManager.newTests.size)
    assertEquals(7, state.testsManager.tests.size)
  }
}

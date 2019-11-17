package tests

import cdgp._
import fuel.core.StatePop
import fuel.func.BestSoFar
import fuel.util.{CallCounter, CollectorStdout, Options, Rng}
import org.junit.Assert._
import org.junit.Test
import swim.tree.Op

class TestValidationSet {
  implicit val emptyOpt = Options(s"--selection lexicase --evolutionMode generational ${Global.solverConfig}")
  implicit val coll = CollectorStdout(emptyOpt)
  implicit val rng = Rng(emptyOpt)
  val code =
    """(set-logic NRA)
      |(synth-fun f ( (x Real)) Real )
      |(constraint (= (f 0.0) 0.0))
      |(constraint (= (f 1.0) 2.0))
      |(check-synth)
      """.stripMargin
  val problem = LoadSygusBenchmark.parseText(code)
  val state = StateCDGP(problem)
  val trainSet = Seq( (Map("x"->0.0), Some(0.0)), (Map("x"->1.0), Some(2.0)) )
  val validSet = Seq( (Map("x"->2.0), Some(4.0)), (Map("x"->3.0), Some(6.0)) )
  val evComplete = EvaluatorCompleteTestsContinuous(state)

  @Test
  def test_validationSet_noImprovement(): Unit = {
    val vst = new ValidationSetTermination[FDouble](trainSet, validSet, evComplete, maxWithoutImprovement=2)
    var bsf = BestSoFar[Op, FDouble](FDoubleOrdering, CallCounter[Any, Any](x => x))

    var s = StatePop(Seq( (Op.fromStr("x"), FDouble(false, 1.0, 1, 2)) ))
    bsf.apply(s)
    assertEquals(false, vst.apply(bsf))  // This is initial call - iterCounter = 0

    bsf.apply(s)
    assertEquals(false, vst.apply(bsf))  // iterCounter = 1

    bsf.apply(s)
    assertEquals(false, vst.apply(bsf))  // iterCounter = 2

    bsf.apply(s)
    assertEquals(false, vst.apply(bsf))  // iterCounter = 3; false because still the same solution

    s = StatePop(Seq( (Op.fromStr("+(x 0.0)"), FDouble(false, 0.0, 1, 2)) ))
    bsf = BestSoFar[Op, FDouble](FDoubleOrdering, CallCounter[Any, Any](x => x))
    bsf.apply(s)
    assertEquals(true, vst.apply(bsf))  // iterCounter = 4
  }


  @Test
  def test_validationSet_resetByImprovement(): Unit = {
    val vst = new ValidationSetTermination[FDouble](trainSet, validSet, evComplete, maxWithoutImprovement=2)
    val bsf = BestSoFar[Op, FDouble](FDoubleOrdering, CallCounter[Any, Any](x => x))

    var s = StatePop(Seq( (Op.fromStr("x"), FDouble(false, 1.0, 1, 2)) ))
    bsf.apply(s)
    assertEquals(false, vst.apply(bsf))  // This is initial call - iterCounter = 0

    bsf.apply(s)
    assertEquals(false, vst.apply(bsf))  // iterCounter = 1

    bsf.apply(s)
    assertEquals(false, vst.apply(bsf))  // iterCounter = 2

    // Here comes better solution and improvement happens
    s = StatePop(Seq( (Op.fromStr("*(2.0 x)"), FDouble(false, 0.0, 1, 2)) ))
    bsf.apply(s)
    assertEquals(false, vst.apply(bsf))  // iterCounter = 0

    bsf.apply(s)
    assertEquals(false, vst.apply(bsf))  // iterCounter = 1

    bsf.apply(s)
    assertEquals(false, vst.apply(bsf))  // iterCounter = 2

    bsf.apply(s)
    assertEquals(false, vst.apply(bsf))  // iterCounter = 3 false because still the same solution

    s = StatePop(Seq( (Op.fromStr("*(+(1.0 1.0) x)"), FDouble(false, 0.0, 1, 2)) ))
    bsf.apply(s)  // bsf is not updated, solution was not strictly better
    assertEquals(false, vst.apply(bsf))  // iterCounter = 4
  }
}

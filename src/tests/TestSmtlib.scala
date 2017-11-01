package tests

import org.junit.Test
import org.junit.Assert._
import cdgp._
import fuel.util.{CollectorStdout, Options, Rng}
import swim.tree.Op

final class TestSmtlib {
  implicit val emptyOpt = Options("--searchAlgorithm Lexicase --solverPath " +
    s"${Global.solverPath}")
  implicit val coll = CollectorStdout(emptyOpt)
  implicit val rng = Rng(emptyOpt)
  val solver = new SolverManager(Global.solverPath, None, verbose=false)
  val specMax =
    """(set-logic LIA)
      |(synth-fun max2 ((x Int) (y Int)) Int)
      |(declare-var x Int)
      |(declare-var y Int)
      |(constraint (>= (max2 x y) x))
      |(constraint (>= (max2 x y) y))
      |(constraint (or (= x (max2 x y))
      |				     (= y (max2 x y))))
      |(check-synth)
    """.stripMargin
  val maxProblem = LoadSygusBenchmark.parseText(specMax)
  val maxSynthTask = SygusSynthesisTask(maxProblem).head
  val maxConstr = SygusBenchmarkConstraints(maxProblem, maxSynthTask)


  @Test
  def test_templateVerification(): Unit = {
    val templateVerification = new QueryTemplateVerification(maxProblem, maxConstr)
    val op = Op.fromStr("ite(=(x y) x y)", useSymbols = false)
    val query = templateVerification(op)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    println(s"Counterexample: $model")

    val op2 = Op.fromStr("ite(>=(x y) x y)", useSymbols = false)
    val query2 = templateVerification(op2)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
    assertEquals(false, model2.isDefined)
  }


  @Test
  def test_templateFindOutput(): Unit = {
    val templateFindOutput = new QueryTemplateFindOutput(maxProblem, maxConstr)
    val inputs = Map("x" -> 5, "y" -> 2)
    val query = templateFindOutput(inputs)
    val (dec, model) = solver.runSolver(query, "(get-value (CorrectOutput))")
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    println(s"Counterexample: $model")
    assertEquals(Map("CorrectOutput" -> 5), GetValueParser(model.get).toMap)
  }


  @Test
  def test_templateInputAndKnownOutput(): Unit = {
    val templateInputAndKnownOutput = new QueryTemplateInputAndKnownOutput(maxProblem, maxConstr)
    val inputs = Map("x" -> 5, "y" -> 2)
    val query = templateInputAndKnownOutput(inputs, 5)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)

    val query2 = templateInputAndKnownOutput(inputs, 2)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
    assertEquals(false, model2.isDefined)
  }


  @Test
  def test_templateInputAndUnknownOutput(): Unit = {
    val templateInputAndUnknownOutput = new QueryTemplateInputAndUnknownOutput(maxProblem, maxConstr)
    val op = Op.fromStr("ite(=(x y) x y)", useSymbols = false)
    val inputs = Map("x" -> 5, "y" -> 2)
    val query = templateInputAndUnknownOutput(op, inputs)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)
    assertEquals(false, model.isDefined)

    val op2 = Op.fromStr("ite(>=(x y) x y)", useSymbols = false)
    val query2 = templateInputAndUnknownOutput(op2, inputs)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("sat", dec2)
  }


  @Test
  def test_singleAnswerProperty(): Unit = {
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(maxSynthTask, maxProblem)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)
    assertEquals(false, model.isDefined)
  }


  @Test
  def test_opToString(): Unit = {
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=false)
    assertEquals("(ite (>= x y) x 0)", SMTLIBFormatter.opToString(op))
    assertEquals("(ite (>= x y) x 0)", SMTLIBFormatter(op))
  }
}

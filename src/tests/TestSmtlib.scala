package tests

import org.junit.Test
import org.junit.Assert._
import cdgp._
import fuel.util.{CollectorStdout, Options, Rng}
import swim.tree.Op

final class TestSmtlib {
  implicit val emptyOpt = Options(s"--searchAlgorithm Lexicase ${Global.solverConfig}")
  implicit val coll = CollectorStdout(emptyOpt)
  implicit val rng = Rng(emptyOpt)
  println("Creating solver.")
  val solver = SolverManager(emptyOpt, coll)
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
  val specMedian =
    """(set-logic LIA)
      |(synth-fun median3 ((a Int) (b Int) (c Int)) Int)
      |(declare-var a Int)
      |(declare-var b Int)
      |(declare-var c Int)
      |; Test cases
      |(constraint (= (median3 0 1 2) 1) )
      |(constraint (= (median3 0 0 0) 0) )
      |; Formal spec
      |(constraint (=> (> a b) (=> (> b c) (= (median3 a b c) b) ) ))
      |(constraint (=> (> a b) (=> (<= b c) (=> (> a c) (= (median3 a b c) c) ) ) ) )
      |(constraint (=> (> a b) (=> (<= b c) (=> (<= a c ) (= (median3 a b c) a) ) ) ) )
      |(constraint (=> (<= a b) (=> (> a c) (= (median3 a b c) a) ) ) )
      |(constraint (=> (<= a b) (=> (<= a c) (=> (> b c) (= (median3 a b c) c) ) ) ) )
      |(constraint (=> (<= a b) (=> (<= a c) (=> (<= b c ) (= (median3 a b c) b) ) ) ) )
      |(check-synth)
    """.stripMargin
  val medianProblem = LoadSygusBenchmark.parseText(specMedian)
  val medianSynthTask = SygusSynthesisTask(medianProblem).head
  val medianConstr = SygusBenchmarkConstraints(medianProblem, medianSynthTask)
  val unitedMax =
    """(set-logic LIA)
      |(synth-fun united ((a Int)) Int)
      |(declare-var x Int)
      |(constraint (= (united 1) 5))
      |(constraint (= (united x) (united (+ x 1))))
      |(check-synth)
    """.stripMargin
  val unitedProblem = LoadSygusBenchmark.parseText(unitedMax)
  val unitedSynthTask = SygusSynthesisTask(unitedProblem).head
  val unitedConstr = SygusBenchmarkConstraints(unitedProblem, unitedSynthTask)


  ////////////////////////////////////////////////////////////////////////////////////
  //             Tests for MAX
  ////////////////////////////////////////////////////////////////////////////////////
  @Test
  def test_templateVerification_max(): Unit = {
    val templateVerification = new TemplateVerification(maxProblem, maxConstr)
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
  def test_templateFindOutput_max(): Unit = {
    val templateFindOutput = new TemplateFindOutput(maxProblem, maxConstr)
    val inputs = Map("x" -> 5, "y" -> 2)
    val query = templateFindOutput(inputs)
    val (dec, model) = solver.runSolver(query, "(get-value (CorrectOutput))")
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    assertEquals(Map("CorrectOutput" -> 5), GetValueParser(model.get).toMap)
  }


  @Test
  def test_templateIsOutputCorrectForInput_max(): Unit = {
    val templateIsOutputCorrectForInput = new TemplateIsOutputCorrectForInput(maxProblem, maxConstr)
    val inputs = Map("x" -> 5, "y" -> 2)
    val query = templateIsOutputCorrectForInput(inputs, 5)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)

    val query2 = templateIsOutputCorrectForInput(inputs, 2)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
    assertEquals(false, model2.isDefined)
  }


  @Test
  def test_templateIsProgramCorrectForInput_max(): Unit = {
    val templateIsProgramCorrectForInput = new TemplateIsProgramCorrectForInput(maxProblem, maxConstr)
    val op = Op.fromStr("ite(=(x y) x y)", useSymbols = false)
    val inputs = Map("x" -> 5, "y" -> 2)
    val query = templateIsProgramCorrectForInput(op, inputs)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)
    assertEquals(false, model.isDefined)

    val op2 = Op.fromStr("ite(>=(x y) x y)", useSymbols = false)
    val query2 = templateIsProgramCorrectForInput(op2, inputs)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("sat", dec2)
  }


  @Test
  def test_singleAnswerProperty_max(): Unit = {
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(maxSynthTask, maxProblem)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)
    assertEquals(false, model.isDefined)
    assertEquals(true, SygusUtils.hasSingleInvocationProperty(maxConstr))
  }


  @Test
  def test_simplify_max(): Unit = {
    val templateSimplify = new TemplateSimplify(maxProblem, maxConstr)
    val query = templateSimplify("(+ x (- 0 x))")
    val res = solver.executeQuery(query)
    assertEquals("0", res)
  }


  ////////////////////////////////////////////////////////////////////////////////////
  //             Tests for MEDIAN
  ////////////////////////////////////////////////////////////////////////////////////
  @Test
  def test_templateVerification_median(): Unit = {
    val templateVerification = new TemplateVerification(medianProblem, medianConstr)
    val op = Op.fromStr("ite(>=(a b) a b)", useSymbols = false)
    val query = templateVerification(op)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    println(s"Counterexample: $model")

    val op2 = SMTLIBFormatter.smtlibToOp( // Solution for Median3 is rather long...
      """(ite (and (or (not (>= a (+ b 1))) (or (>= b (+ c 1)) (or (not (>= a (+ c 1))) (= b c))))
        |(and (or (not (>= a (+ b 1))) (or (>= b (+ c 1)) (>= a (+ c 1)))) (and (or (>= a (+ b 1))
        |(or (not (>= a (+ c 1))) (= a b))) (or (>= a (+ b 1)) (or (>= a (+ c 1)) (not (>= b (+ c 1)
        |))))))) b (ite (and (or (not (>= a (+ b 1))) (not (>= b (+ c 1)))) (and (or (not (>= a (+ b 1)))
        |(or (>= b (+ c 1)) (or (>= a (+ c 1)) (= a c)))) (and (or (>= a (+ b 1)) (not (>= a (+ c 1))))
        |(or (>= a (+ b 1)) (or (>= a (+ c 1)) (or (>= b (+ c 1)) (= b c))))))) c a))""".stripMargin)
    println("op2:\n" + op2)
    val query2 = templateVerification(op2)
    println("query2:\n" + query2)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
    assertEquals(false, model2.isDefined)
  }


  @Test
  def test_templateFindOutput_median(): Unit = {
    val templateFindOutput = new TemplateFindOutput(medianProblem, medianConstr)
    val inputs = Map("a" -> 5, "b" -> 2, "c" -> 2)
    val query = templateFindOutput(inputs)
    val (dec, model) = solver.runSolver(query, "(get-value (CorrectOutput))")
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    assertEquals(Map("CorrectOutput" -> 2), GetValueParser(model.get).toMap)
  }


  @Test
  def test_templateIsOutputCorrectForInput_median(): Unit = {
    val templateIsOutputCorrectForInput = new TemplateIsOutputCorrectForInput(medianProblem, medianConstr)
    val inputs = Map("a" -> 5, "b" -> 2, "c" -> 2)
    val query = templateIsOutputCorrectForInput(inputs, 2) // correct
    println("query:\n" + query)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)

    val query2 = templateIsOutputCorrectForInput(inputs, 5) // incorrect
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
    assertEquals(false, model2.isDefined)
  }


  @Test
  def test_templateIsProgramCorrectForInput_median(): Unit = {
    val templateIsProgramCorrectForInput = new TemplateIsProgramCorrectForInput(medianProblem, medianConstr)
    val op = Op.fromStr("ite(>=(a b) a b)", useSymbols = false) // incorrect
    val inputs = Map("a" -> 5, "b" -> 2,  "c" -> 2)
    val query = templateIsProgramCorrectForInput(op, inputs)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)
    assertEquals(false, model.isDefined)

    val op2 = Op.fromStr("2", useSymbols = false) // correct (on this input)
    val query2 = templateIsProgramCorrectForInput(op2, inputs)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("sat", dec2)
  }


  @Test
  def test_singleAnswerProperty_median(): Unit = {
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(medianSynthTask, medianProblem)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)
    assertEquals(false, model.isDefined)
    assertEquals(true, SygusUtils.hasSingleInvocationProperty(medianConstr))
  }






  ////////////////////////////////////////////////////////////////////////////////////
  //             Tests for UNITED
  ////////////////////////////////////////////////////////////////////////////////////
  @Test
  def test_templateVerification_united(): Unit = {
    val templateVerification = new TemplateVerification(unitedProblem, unitedConstr)
    val op = Op.fromStr("a", useSymbols = false)
    val query = templateVerification(op)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    println(s"Counterexample: $model")

    val op2 = Op.fromStr("5", useSymbols = false)
    val query2 = templateVerification(op2)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
    assertEquals(false, model2.isDefined)
  }


  @Test
  def test_templateFindOutput_united(): Unit = {
    val templateFindOutput = new TemplateFindOutput(unitedProblem, unitedConstr)
    val inputs = Map("x" -> 0)
    val query = templateFindOutput(inputs)
    println("query:\n" + query)
    val (dec, model) = solver.runSolver(query, "(get-value (CorrectOutput))")
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    assertEquals(Map("CorrectOutput" -> 5), GetValueParser(model.get).toMap)
  }


  @Test
  def test_templateIsOutputCorrectForInput_united(): Unit = {
    val templateIsOutputCorrectForInput = new TemplateIsOutputCorrectForInput(unitedProblem, unitedConstr)
    val inputs = Map("x" -> 5)
    val query = templateIsOutputCorrectForInput(inputs, 5) // correct
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)

    val query2 = templateIsOutputCorrectForInput(inputs, 2) // incorrect
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
    assertEquals(false, model2.isDefined)
  }


  @Test
  def test_templateIsProgramCorrectForInput_united(): Unit = {
    val templateIsProgramCorrectForInput = new TemplateIsProgramCorrectForInput(unitedProblem, unitedConstr)
    val op = Op.fromStr("a", useSymbols = false) // incorrect
    val inputs = Map("x" -> 3)
    val query = templateIsProgramCorrectForInput(op, inputs)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)
    assertEquals(false, model.isDefined)

    val op2 = Op.fromStr("5", useSymbols = false) // correct
    val query2 = templateIsProgramCorrectForInput(op2, inputs)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("sat", dec2)
  }


  @Test
  def test_singleAnswerProperty_united(): Unit = {
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(unitedSynthTask, unitedProblem)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)
    assertEquals(false, model.isDefined)
    assertEquals(false, SygusUtils.hasSingleInvocationProperty(unitedConstr))
  }







  ////////////////////////////////////////////////////////////////////////////////////
  //             Other tests
  ////////////////////////////////////////////////////////////////////////////////////

  @Test
  def test_opToString(): Unit = {
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=false)
    assertEquals("(ite (>= x y) x 0)", SMTLIBFormatter.opToString(op))
    assertEquals("(ite (>= x y) x 0)", SMTLIBFormatter(op))
  }
}

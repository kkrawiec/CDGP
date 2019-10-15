package tests

import org.junit.Test
import org.junit.Assert._
import cdgp._
import fuel.util.{CollectorStdout, Options, Rng}
import swim.tree.Op

final class TestSmtlib {
  implicit val emptyOpt = Options(s"--selection lexicase --evolutionMode generational ${Global.solverConfig}")
  implicit val coll = CollectorStdout(emptyOpt)
  implicit val rng = Rng(emptyOpt)
  println("Creating solver.")
  val solver = SolverManager(emptyOpt, coll)
  val specMax =
    """(set-logic NIA)
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
  val maxData = SygusProblemData(maxProblem)
  val specMaxPartial =
    """(set-logic NIA)
      |(synth-fun max2 ((x Int) (y Int)) Int)
      |(declare-var x Int)
      |(constraint (>= (max2 x 1) x))
      |(constraint (>= (max2 x 1) 1))
      |(constraint (or (= x (max2 x 1))
      |				     (= 1 (max2 x 1))))
      |(check-synth)
    """.stripMargin
  val maxPartialProblem = LoadSygusBenchmark.parseText(specMaxPartial)
  val maxPartialData = SygusProblemData(maxPartialProblem)
  val specMedian =
    """(set-logic NIA)
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
  val medianData = SygusProblemData(medianProblem)
  val unitedMax =
    """(set-logic NIA)
      |(synth-fun united ((a Int)) Int)
      |(declare-var x Int)
      |(constraint (= (united 1) 5))
      |(constraint (= (united x) (united (+ x 1))))
      |(check-synth)
    """.stripMargin
  val unitedProblem = LoadSygusBenchmark.parseText(unitedMax)
  val unitedData = SygusProblemData(unitedProblem)



  ////////////////////////////////////////////////////////////////////////////////////
  //             Tests for MAX
  ////////////////////////////////////////////////////////////////////////////////////
  @Test
  def test_templateVerification_max(): Unit = {
    val templateVerification = new TemplateVerification(maxData)
    val op = Op.fromStr("ite(=(x y) x y)", useSymbols = true)
    val query = templateVerification(op)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    println(s"Counterexample: $model")

    val op2 = Op.fromStr("ite(>=(x y) x y)", useSymbols = true)
    val query2 = templateVerification(op2)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)

    // what if a correct solution has a potential division by 0?
    val op3 = Op.fromStr("ite(>=(x y) (div (* x x) x) y)", useSymbols = true)
    val query3 = templateVerification(op3)
    val (dec3, model3) = solver.runSolver(query3)
    assertEquals("sat", dec3)  // This is actually desired in this case, because sat is returned while program is incorrect
    assertEquals(Some(0), GetValueParser(model3.get).toMap.get("x"))
  }

  @Test
  def test_templateFindOutput_max(): Unit = {
    val templateFindOutput = new TemplateFindOutput(maxData)
    val inputs = Map("x" -> 5, "y" -> 2)
    val query = templateFindOutput(inputs)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    assertEquals(Map("CorrectOutput" -> 5), GetValueParser(model.get).toMap)
    // Try to find other correct output
    val query2 = templateFindOutput(inputs, Seq(5))
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
  }

  @Test
  def test_templateIsOutputCorrectForInput_max(): Unit = {
    val templateIsOutputCorrectForInput = new TemplateIsOutputCorrectForInput(maxData)
    val inputs = Map("x" -> 5, "y" -> 2)
    val query = templateIsOutputCorrectForInput(inputs, 5)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)

    val query2 = templateIsOutputCorrectForInput(inputs, 2)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
  }

  @Test
  def test_templateIsProgramCorrectForInput_max(): Unit = {
    val templateIsProgramCorrectForInput = new TemplateIsProgramCorrectForInput(maxData)
    val op = Op.fromStr("ite(=(x y) x y)", useSymbols = true)
    val inputs = Map("x" -> 5, "y" -> 2)
    val query = templateIsProgramCorrectForInput(op, inputs)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)

    val op2 = Op.fromStr("ite(>=(x y) x y)", useSymbols = true)
    val query2 = templateIsProgramCorrectForInput(op2, inputs)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("sat", dec2)

    // what if an incorrect solution has a potential division by 0?
    val inputs3 = Map("x" -> 5, "y" -> 0)
    val op3 = Op.fromStr("ite(>=(x y) (div (* y y) y) y)", useSymbols = true)
    val query3 = templateIsProgramCorrectForInput(op3, inputs3)
    val (dec3, model3) = solver.runSolver(query3)
    assertEquals("sat", dec3)  // This is undesired in this case, program is incorrect, but it is claimed to work correct for this input
  }

  @Test
  def test_singleAnswerProperty_max(): Unit = {
    assertEquals(true, maxData.singleInvocFormal)
    assertEquals(true, maxData.singleInvocAll)
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(maxProblem, maxData)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)
  }

  @Test
  def test_simplify_max(): Unit = {
    val templateSimplify = new TemplateSimplify(maxData)
    val query = templateSimplify("(+ x (- 0 x))")
    println(query)
    val res = solver.executeQuery(query)
    assertEquals("0", res)
  }





  ////////////////////////////////////////////////////////////////////////////////////
  //             Tests for MAX PARTIAL
  ////////////////////////////////////////////////////////////////////////////////////
  @Test
  def test_templateVerification_maxPartial(): Unit = {
    val templateVerification = new TemplateVerification(maxPartialData)
    val op = Op.fromStr("ite(=(x y) x y)", useSymbols = true)
    val query = templateVerification(op)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    println(s"Counterexample: $model")

    val op2 = Op.fromStr("ite(>=(x y) x y)", useSymbols = true)
    val query2 = templateVerification(op2)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
  }

  @Test
  def test_templateFindOutput_maxPartial(): Unit = {
    val templateFindOutput = new TemplateFindOutput(maxPartialData)
    val inputs = Map("x" -> 5, "y" -> 2)
    val query = templateFindOutput(inputs)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    assertEquals(Map("CorrectOutput" -> 5), GetValueParser(model.get).toMap)
  }

  @Test
  def test_templateIsOutputCorrectForInput_maxPartial(): Unit = {
    val templateIsOutputCorrectForInput = new TemplateIsOutputCorrectForInput(maxPartialData)
    val inputs = Map("x" -> 5, "y" -> 2)
    val query = templateIsOutputCorrectForInput(inputs, 5)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)

    val query2 = templateIsOutputCorrectForInput(inputs, 1)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
  }

  @Test
  def test_templateIsProgramCorrectForInput_maxPartial(): Unit = {
    val templateIsProgramCorrectForInput = new TemplateIsProgramCorrectForInput(maxPartialData)
    val op = Op.fromStr("ite(=(x y) x y)", useSymbols = true)
    val inputs = Map("x" -> 5, "y" -> 2)
    val query = templateIsProgramCorrectForInput(op, inputs)
    println(query)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)

    val op2 = Op.fromStr("ite(>=(x y) x y)", useSymbols = true)
    val query2 = templateIsProgramCorrectForInput(op2, inputs)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("sat", dec2)
  }

  @Test
  def test_singleAnswerProperty_maxPartial(): Unit = {
    assertEquals(true, maxPartialData.singleInvocFormal)
    assertEquals(true, maxPartialData.singleInvocAll)
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(maxPartialProblem, maxPartialData)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)
  }





  ////////////////////////////////////////////////////////////////////////////////////
  //             Tests for MEDIAN
  ////////////////////////////////////////////////////////////////////////////////////
  @Test
  def test_templateVerification_median(): Unit = {
    val templateVerification = new TemplateVerification(medianData)
    val op = Op.fromStr("ite(>=(a b) a b)", useSymbols = true)
    val query = templateVerification(op)
    println(query)
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
  }

  @Test
  def test_templateFindOutput_median(): Unit = {
    val templateFindOutput = new TemplateFindOutput(medianData)
    val inputs = Map("a" -> 5, "b" -> 2, "c" -> 2)
    val query = templateFindOutput(inputs)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    assertEquals(Map("CorrectOutput" -> 2), GetValueParser(model.get).toMap)
    // Try to find other correct output
    val query2 = templateFindOutput(inputs, Seq(2))
    val (dec2, _) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
  }

  @Test
  def test_templateIsOutputCorrectForInput_median(): Unit = {
    val templateIsOutputCorrectForInput = new TemplateIsOutputCorrectForInput(medianData)
    val inputs = Map("a" -> 5, "b" -> 2, "c" -> 2)
    val query = templateIsOutputCorrectForInput(inputs, 2) // correct
    println("query:\n" + query)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)

    val query2 = templateIsOutputCorrectForInput(inputs, 5) // incorrect
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
  }

  @Test
  def test_templateIsProgramCorrectForInput_median(): Unit = {
    val templateIsProgramCorrectForInput = new TemplateIsProgramCorrectForInput(medianData)
    val op = Op.fromStr("ite(>=(a b) a b)", useSymbols = true) // incorrect
    val inputs = Map("a" -> 5, "b" -> 2,  "c" -> 2)
    val query = templateIsProgramCorrectForInput(op, inputs)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)
    assertEquals(false, model.isDefined)

    val op2 = Op.fromStr("2", useSymbols = true) // correct (on this input)
    val query2 = templateIsProgramCorrectForInput(op2, inputs)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("sat", dec2)
  }

  @Test
  def test_singleAnswerProperty_median(): Unit = {
    assertEquals(true, medianData.singleInvocFormal)
    assertEquals(false, medianData.singleInvocAll)
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(medianProblem, medianData)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)
  }





  ////////////////////////////////////////////////////////////////////////////////////
  //             Tests for UNITED
  ////////////////////////////////////////////////////////////////////////////////////
  @Test
  def test_templateVerification_united(): Unit = {
    val templateVerification = new TemplateVerification(unitedData)
    val op = Op.fromStr("a", useSymbols = true)
    val query = templateVerification(op)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    println(s"Counterexample: $model")

    val op2 = Op.fromStr("5", useSymbols = true)
    val query2 = templateVerification(op2)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
  }

  @Test
  def test_templateFindOutput_united(): Unit = {
    val templateFindOutput = new TemplateFindOutput(unitedData)
    val inputs = Map("x" -> 0)
    try { templateFindOutput(inputs); fail() }
    catch { case e: AssertionError => }
  }

  @Test
  def test_templateIsOutputCorrectForInput_united(): Unit = {
    val templateIsOutputCorrectForInput = new TemplateIsOutputCorrectForInput(unitedData)
    val inputs = Map("x" -> 5)
    try { templateIsOutputCorrectForInput(inputs, 5); fail() }
    catch { case e: AssertionError => }
  }

  @Test
  def test_templateIsProgramCorrectForInput_united(): Unit = {
    val templateIsProgramCorrectForInput = new TemplateIsProgramCorrectForInput(unitedData)
    val op = Op.fromStr("a", useSymbols = true) // incorrect
    val inputs = Map("x" -> 3)
    val query = templateIsProgramCorrectForInput(op, inputs)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)
    assertEquals(false, model.isDefined)

    val op2 = Op.fromStr("5", useSymbols = true) // correct
    val query2 = templateIsProgramCorrectForInput(op2, inputs)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("sat", dec2)
  }

  @Test
  def test_singleAnswerProperty_united(): Unit = {
    assertEquals(false, unitedData.singleInvocFormal)
    assertEquals(false, unitedData.singleInvocAll)
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(unitedProblem, unitedData,
      useAllConstraints=false)
    val (dec, _) = solver.runSolver(query)
    assertEquals("sat", dec)

    val query2 = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(unitedProblem, unitedData,
      useAllConstraints=true)
    val (dec2, _) = solver.runSolver(query2)
    assertEquals("unsat", dec2)  // unsat, because test cases constraints bound possibilities
  }





  ////////////////////////////////////////////////////////////////////////////////////
  //             Other tests
  ////////////////////////////////////////////////////////////////////////////////////

  @Test
  def test_opToString(): Unit = {
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=true)
    assertEquals("(ite (>= x y) x 0)", SMTLIBFormatter.opToString(op))
    assertEquals("(ite (>= x y) x 0)", SMTLIBFormatter(op))
    val op2 = Op.fromStr("ite(>=(x y) x \"0\")", useSymbols=true)
    assertEquals("(ite (>= x y) x \"0\")", SMTLIBFormatter.opToString(op2))
    val op3 = Op('default, 3.0e-10)
    assertEquals("0.0000000003", SMTLIBFormatter.opToString(op3))
  }

  @Test
  def test_constToSmtlib(): Unit = {
    assertEquals("true", SMTLIBFormatter.constToSmtlib(true))
    assertEquals("false", SMTLIBFormatter.constToSmtlib(false))
    assertEquals("123", SMTLIBFormatter.constToSmtlib(123))
    assertEquals("0.345", SMTLIBFormatter.constToSmtlib(0.345))
    assertEquals("0.00000000003", SMTLIBFormatter.constToSmtlib(0.3e-10))
    assertEquals("\"asd\"", SMTLIBFormatter.constToSmtlib("asd"))
    assertEquals("\"\"", SMTLIBFormatter.constToSmtlib(""))
  }

  @Test
  def test_testsAsIteExpr(): Unit = {
    val t1 = (Map("a"->5), 11)
    val t2 = (Map("a"->0), 10)  // names are the same to avoid problems with order
    assertEquals("(ite (and (= a 5)) 11 (ite (and (= a 0)) 10 20))",
      SMTLIBFormatter.testsAsIteExpr(Seq(t1, t2), "20"))
  }

  @Test
  def test_smtlibToOp1(): Unit = {
    val s = """(str.substr name 0 (str.indexof name " " 0))"""
    assertEquals(Op(Symbol("str.substr"), Op('name), Op(0), Op(Symbol("str.indexof"), Op('name), Op(" "), Op(0))),
                 SMTLIBFormatter.smtlibToOp(s))
  }

  @Test
  def test_smtlibToOp2(): Unit = {
    assertEquals(Op(""), SMTLIBFormatter.smtlibToOp("\"\""))
    assertEquals(Op(" "), SMTLIBFormatter.smtlibToOp("\" \""))
    assertEquals(Op("  "), SMTLIBFormatter.smtlibToOp("\"  \""))
    assertEquals(Op("a  b"), SMTLIBFormatter.smtlibToOp("\"a  b\""))
  }
}

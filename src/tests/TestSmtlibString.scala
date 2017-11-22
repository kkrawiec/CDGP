package tests

import cdgp._
import fuel.util.{CollectorStdout, Options, Rng}
import org.junit.Assert._
import org.junit.Test
import swim.tree.Op

final class TestSmtlibString {
  implicit val emptyOpt = Options(s"--searchAlgorithm Lexicase ${Global.solverConfig}")
  implicit val coll = CollectorStdout(emptyOpt)
  implicit val rng = Rng(emptyOpt)
  println("Creating solver.")
  val solver = SolverManager(emptyOpt, coll)
  val firstnameProblem = LoadSygusBenchmark.parseText(Global.specFirstname)
  val firstnameData = SygusProblemData(firstnameProblem)




  ////////////////////////////////////////////////////////////////////////////////////
  //             Tests for FIRSTNAME
  ////////////////////////////////////////////////////////////////////////////////////
  @Test
  def test_templateVerification_firstname(): Unit = {
    val templateVerification = new TemplateVerification(firstnameProblem, firstnameData)
    val op = Op.fromStr("str.at(name 0)", useSymbols = true)
    val query = templateVerification(op)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    println(s"Counterexample: $model")

    val op2 = """(str.substr name 0 (str.indexof name " " 0))"""
    val query2 = templateVerification(op2)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
  }

  @Test
  def test_templateFindOutput_firstname(): Unit = {
    val templateFindOutput = new TemplateFindOutput(firstnameProblem, firstnameData)
    val inputs = Map("s" -> "Iwo Bladek")
    val query = templateFindOutput(inputs)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)
    assertEquals(true, model.isDefined)
    assertEquals(Map("CorrectOutput" -> "Iwo"), GetValueParser(model.get).toMap)
    // Try to find other correct output
    val query2 = templateFindOutput(inputs, Seq("Iwo"))
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
  }

  @Test
  def test_templateIsOutputCorrectForInput_firstname(): Unit = {
    val templateIsOutputCorrectForInput = new TemplateIsOutputCorrectForInput(firstnameProblem, firstnameData)
    val inputs = Map("s" -> "Iwo Bladek")
    val query = templateIsOutputCorrectForInput(inputs, "Iwo")
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)

    val query2 = templateIsOutputCorrectForInput(inputs, "Bladek")
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("unsat", dec2)
  }

  @Test
  def test_templateIsProgramCorrectForInput_firstname(): Unit = {
    val templateIsProgramCorrectForInput = new TemplateIsProgramCorrectForInput(firstnameProblem, firstnameData)
    val op = "(str.substr name 0 1)"
    val inputs = Map("s" -> "Iwo Bladek")
    val query = templateIsProgramCorrectForInput(op, inputs)
    val (dec, model) = solver.runSolver(query)
    assertEquals("unsat", dec)  // incorrect

    val op2 = """(str.substr name 0 (str.indexof name " " 0))"""
    val query2 = templateIsProgramCorrectForInput(op2, inputs)
    val (dec2, model2) = solver.runSolver(query2)
    assertEquals("sat", dec2)  // correct
  }

  @Test
  def test_singleAnswerProperty_firstname(): Unit = {
    assertEquals(true, firstnameData.singleInvocFormal)
    assertEquals(true, firstnameData.singleInvocAll)
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(firstnameProblem, firstnameData)
    val (dec, model) = solver.runSolver(query)
    assertEquals("sat", dec)
  }

  @Test
  def test_simplify_firstname(): Unit = {
    val templateSimplify = new TemplateSimplify(firstnameProblem, firstnameData)
    val query = templateSimplify("(str.++ \"asd\" \"\")")
    println(query)
    val res = solver.executeQuery(query)
    assertEquals("\"asd\"", res)
  }

}

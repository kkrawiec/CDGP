package tests

import cdgp._
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
(synth-fun max2 ((argA Int) (argB Int)) Int
  ((Start Int (argA argB 0 1
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
(constraint (= (f x) (+ 10 (f x))))
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
  implicit val emptyOpt = Options(s"--selection lexicase --evolutionMode generational ${Global.solverConfig}")
  implicit val coll = CollectorStdout(emptyOpt)
  implicit val rng = Rng(emptyOpt)

  @Test
  def test_max2_t(): Unit = {
    // Testing CDGP for pure test-based specification
    val state = CDGPState("resources/LIA/tests/max2_t.sl")
    val cdgpFit = CDGPFitnessD(state)
    state.testsManager.flushHelpers()  // propagate tests
    assertEquals(5, state.testsManager.getNumberOfTests)
    assertEquals(5, state.testsManager.getNumberOfKnownOutputs)
    assertEquals(false, state.sygusData.singleInvocAll)
    assertEquals(false, state.sygusData.singleInvocFormal)

    val op = SMTLIBFormatter.smtlibToOp("""(ite (>= a b) a b)""")
    assertEquals(Set("a", "b"), state.testsManager.getTests().head._1.keys.toSet)
    assertEquals(0, cdgpFit.evalOnTests(op, state.testsManager.getTests()).sum)
  }

  @Test
  def test_evalOnTestsMax(): Unit = {
    val code = TestCDGPState.scriptMax
    val problem = LoadSygusBenchmark.parseText(code)
    val fit = CDGPFitnessD(problem)
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=true)
    val t1 = (GetValueParser("((x 4)(y 3))").toMap, Some(4))
    val t2 = (GetValueParser("((x 5)(y 1))").toMap, Some(5))
    val t3 = (GetValueParser("((x 1)(y 3))").toMap, Some(3))
    val tests = Seq(t1, t2, t3)
    assertEquals(Seq(0, 0, 1), fit.evalOnTests(op, tests))
  }

  @Test
  def test_evalOnTestsMaxUsingSolver(): Unit = {
    val problem = LoadSygusBenchmark.parseText(TestCDGPState.scriptMax)
    val state = new CDGPState(problem)
    val fit = CDGPFitnessD(state)
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=true)
    val t1 = (GetValueParser("((x 4)(y 3))").toMap, Some(4))
    val t2 = (GetValueParser("((x 5)(y 1))").toMap, Some(5))
    val t3 = (GetValueParser("((x 1)(y 3))").toMap, Some(3))
    state.testsManager.addNewTest(t1)
    state.testsManager.addNewTest(t2)
    state.testsManager.addNewTest(t3)
    state.testsManager.flushHelpers()
    assertEquals(3, state.testsManager.getNumberOfTests)
    assertEquals(3, state.testsManager.getNumberOfKnownOutputs)
    assertEquals(1, fit.evalOnTests(op, state.testsManager.getTests()).sum)
    assertEquals(3, state.testsManager.getNumberOfKnownOutputs)
//    assertEquals(Some(4), state.testsManager.tests(t1._1))
//    assertEquals(Some(5), state.testsManager.tests(t2._1))
//    assertEquals(None,state.testsManager.tests(t3._1))
  }

  @Test
  def test_evalOnTestsString(): Unit = {
    val problem = LoadSygusBenchmark.parseText(Global.specFirstname)
    val state = new CDGPState(problem)
    val fit = CDGPFitnessD(state)
    val tests = Seq(
      (Map("name" -> "\\x00 \\x00"), Some("\\x00")),
      (Map("name" -> " "),Some("")),
      (Map("name" -> "\\x00 "),Some("\\x00")),
      (Map("name" -> " \\x00"),Some("")),
      (Map("name" -> " \\x00\\x00\\x00"),Some("")),
      (Map("name" -> "\\x00\\x00 \\x00"),Some("\\x00\\x00")),
      (Map("name" -> " \\x00\\x00"),Some("")),
      (Map("name" -> "\\x00 \\x00\\x00"),Some("\\x00")),
      (Map("name" -> " \\x00"),Some("")))
    tests.foreach{ t => state.testsManager.tests += t }
    val op = SMTLIBFormatter.smtlibToOp("""(str.substr name 0 (str.indexof name " " 0))""")
    assertEquals(0, fit.evalOnTests(op, state.testsManager.getTests()).sum)
  }


  @Test
  def test_evalOnTestsMaxVerify(): Unit = {
    val problem = LoadSygusBenchmark.parseText(TestCDGPState.scriptMax)
    val state = new CDGPState(problem)
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=true)
    val (dec, output) = state.verify(op)
    assertEquals("sat", dec)
    assertEquals(true, output.isDefined)
  }

  @Test
  def test_evalOnTestsMaxDifferentVarOrderInModel(): Unit = {
    val problem = LoadSygusBenchmark.parseText(TestCDGPState.scriptMax)
    val fit = CDGPFitnessD(problem)
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=true)
    val t1 = (GetValueParser("((y 3)(x 4))").toMap, Some(4))
    val t2 = (GetValueParser("((y 1)(x 5))").toMap, Some(5))
    val t3 = (GetValueParser("((y 3)(x 1))").toMap, Some(3))
    val tests = Seq(t1, t2, t3)
    assertEquals(Seq(0, 0, 1), fit.evalOnTests(op, tests))
  }

  @Test
  def test_evalOnTestsMaxRenamedVars(): Unit = {
    val problem = LoadSygusBenchmark.parseText(TestCDGPState.scriptMaxRenamedVars)
    val state = new CDGPState(problem)
    val fit = CDGPFitnessD(state)
    val op = Op.fromStr("ite(>=(a b) a 0)", useSymbols=true)
    val t1 = state.createCompleteTest(GetValueParser("((x 4)(y 3))").toMap, Some(4))
    val t2 = state.createCompleteTest(GetValueParser("((x 5)(y 1))").toMap, Some(5))
    val t3 = state.createCompleteTest(GetValueParser("((x 1)(y 3))").toMap, Some(3))
    val tests = Seq(t1, t2, t3)
    assertEquals(Seq(0, 0, 1), fit.evalOnTests(op, tests))

    val t2_2 = (GetValueParser("((y 1)(x 5))").toMap, Some(5))
    val tests_2 = Seq(t1, t2_2, t3)
    assertEquals(Seq(0, 0, 1), fit.evalOnTests(op, tests))
  }

  @Test
  def test_evalOnTestsMaxFixedX(): Unit = {
    val problem = LoadSygusBenchmark.parseText(TestCDGPState.scriptMaxFixedX)
    val state = new CDGPState(problem)
    val fit = CDGPFitnessD(state)
    val op = Op.fromStr("ite(>=(argA argB) argA 0)", useSymbols=true)
    val t1 = state.createCompleteTest(GetValueParser("((asd 4)(y -3))").toMap, Some(1))
    val t2 = state.createCompleteTest(GetValueParser("((asd 5)(y 0))").toMap, Some(1))
    val t3 = state.createCompleteTest(GetValueParser("((asd 1)(y 3))").toMap, Some(3))
    val tests = Seq(t1, t2, t3)
    assertEquals(Seq(0, 0, 1), fit.evalOnTests(op, tests))
  }

  @Test
  def test_evalOnTestsMaxFixedX2(): Unit = {
    val problem = LoadSygusBenchmark.parseText(TestCDGPState.scriptMaxFixedX)
    val state = new CDGPState(problem)
    val fit = CDGPFitnessD(state)
    val op = Op.fromStr("ite(>=(argA argB) argA 0)", useSymbols=true)
    val t1 = state.createCompleteTest(GetValueParser("((y -3))").toMap, Some(1))
    val t2 = state.createCompleteTest(GetValueParser("((y 0))").toMap, Some(1))
    val t3 = state.createCompleteTest(GetValueParser("((y 3))").toMap, Some(3))
    val tests = Seq(t1, t2, t3)
    assertEquals(Seq(0, 0, 1), fit.evalOnTests(op, tests))
  }

  @Test
  def test_checkIfSingleCorrectAnswer_unsat(): Unit = {
    val problem = LoadSygusBenchmark.parseText(TestCDGPState.scriptMaxRenamedVars)
    val sygusData = SygusProblemData(problem)
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(problem, sygusData)
    val state = new CDGPState(problem)
    val (decision, output) = state.solver.runSolver(query)
    assertEquals("unsat", decision)  // unsat, so there is only a single answer
  }

  @Test
  def test_checkIfSingleInvocation(): Unit = {
    val problem = LoadSygusBenchmark.parseText(TestCDGPState.scriptNotSingleInvocation)
    val data = SygusProblemData(problem, mixedSpecAllowed=true)
    assertEquals(true, data.singleInvocFormal)
  }

  @Test
  def test_checkIfSingleCorrectAnswer_sat(): Unit = {
    val problem = LoadSygusBenchmark.parseText(TestCDGPState.scriptPsuedoMaxRenamedVars)
    val sygusData = SygusProblemData(problem)
    val query = SMTLIBFormatter.checkIfSingleAnswerForEveryInput(problem, sygusData)
    val state = new CDGPState(problem)
    val (decision, output) = state.solver.runSolver(query)
    assertEquals("sat", decision)
  }

  @Test
  def test_createRandomTest(): Unit = {
    val code =
      """(set-logic LIA)
        |(synth-fun f ( (w Int)(x Int)(y Int)(z Int)) Int )
        |(declare-var a Int)
        |(constraint (= (f a a 4 4) (+ (* 2 a) 8)))
        |(check-synth)
      """.stripMargin
    val problem = LoadSygusBenchmark.parseText(code)
    val state = new CDGPState(problem)
    val test = state.createRandomTest().get
    assertEquals(true, test.isCompleteTest)
    val test2 = (test._1.map{ case (k, v) => (k, if (k == "y" || k == "z") 4 else 1)}, test._2)
    println(s"Test: $test")
    assertEquals(Seq("a", "a", "4", "4"), state.invocations.head)
    assertEquals(Map("w"->1, "x"->1, "y"->4, "z"->4), test2._1)
  }

  @Test
  def test_createTestFromFailedVerification(): Unit = {
    val code =
      """(set-logic LIA)
        |(synth-fun f ( (w Int)(x Int)(y Int)(z Int)) Int )
        |(declare-var a Int)
        |(constraint (= (f a a 4 4) (+ (* 2 a) 8)))
        |(check-synth)
      """.stripMargin
    val problem = LoadSygusBenchmark.parseText(code)
    val state = new CDGPState(problem)
    val solverOut = "((a 1))"
    val test = state.createTestFromFailedVerification(solverOut).get
    println(s"Test: $test")
    val testModel = GetValueParser(solverOut).toMap
    val testInputs = state.modelToSynthFunInputs(testModel)
    assertEquals(Seq("a", "a", "4", "4"), state.invocations.head)
    assertEquals(Map("w"->1, "x"->1, "y"->4, "z"->4), testInputs)
  }

  @Test
  def test_createTestFromFailedVerification_tooBig(): Unit = {
    val code =
      """(set-logic LIA)
        |(synth-fun f ( (w Int)(x Int)(y Int)(z Int)) Int )
        |(declare-var a Int)
        |(constraint (= (f a a 4 4) (+ (* 2 a) 8)))
        |(check-synth)
      """.stripMargin
    val problem = LoadSygusBenchmark.parseText(code)
    val state = new CDGPState(problem)
    val solverOut = "((a 12345678901234))"
    val test = state.createTestFromFailedVerification(solverOut)
    assertEquals(None, test)
  }

  @Test
  def test_createTestsFromConstraints(): Unit = {
    val code =
      """(set-logic SLIA)
        |(synth-fun f ((s String)(a Int)(b Int)) String ((Start String (s))))
        |(declare-var s String)
        |(declare-var a Int)
        |(declare-var b Int)
        |(constraint (= (f "asd" 0 1) "sad"))
        |(constraint (= (str.len (f s a b)) (str.len s)))
        |(constraint (= (f "asd" 0 2) "das"))
      """.stripMargin
    val problem = LoadSygusBenchmark.parseText(code)
    val data = SygusProblemData(problem, mixedSpecAllowed = true)
    val tests = data.testCasesConstrToTests
    assertEquals(2, tests.size)
    assertEquals(Map("s"->"asd", "a"->0, "b"->1), tests(0)._1)
    assertEquals(Some("sad"), tests(0)._2)
    assertEquals(Map("s"->"asd", "a"->0, "b"->2), tests(1)._1)
    assertEquals(Some("das"), tests(1)._2)
  }

  @Test
  def test_createTestsFromConstraints2(): Unit = {
    val code =
      """(set-logic SLIA)
        |(synth-fun f ((s String)(a Int)(b Int)) String ((Start String (s))))
        |(declare-var s String)
        |(declare-var a Int)
        |(declare-var b Int)
        |(constraint (= (f s 0 0) s))
        |(constraint (= (str.len (f s a b)) (str.len s)))
        |(constraint (= "das" (f "asd" 0 2)))
      """.stripMargin
    val problem = LoadSygusBenchmark.parseText(code)
    val data = SygusProblemData(problem, mixedSpecAllowed = true)
    val tests = data.testCasesConstrToTests
    assertEquals(1, tests.size)
    assertEquals(Map("s"->"asd", "a"->0, "b"->2), tests(0)._1)
    assertEquals(Some("das"), tests(0)._2)
  }

  @Test
  def test_createTestFromCounterex(): Unit = {
    val state = CDGPState("resources/LIA/max2_t.sl")
    val model = Map("x"->5, "y"->9)
    val test = state.createTestFromCounterex(model)
    assertEquals(false, test.isCompleteTest)
    assertEquals(Map("x"->5, "y"->9), test.input)
  }

  @Test
  def test_comprehensive(): Unit = {
    def testBenchmark(path: String, singleAnswerF: Boolean, numTests: Int, numFormConstr: Int, tcInput: Map[String, Any], tcInitialInput: Option[Set[String]]): Unit = {
      println("-" * 50)
      println("FILE: " + path)
      println("-" * 50)
      val state = CDGPState(path)
      // A newly created test, which happens after the verification.
      val test = state.createTestFromCounterex(Map("x"->5, "y"->9))
      assertEquals(singleAnswerF, state.singleAnswerFormal)
      assertEquals(numTests, state.testsManager.newTests.size)
      if (state.testsManager.newTests.nonEmpty) {
        state.testsManager.newTests.foreach { tc =>
          assertEquals(tcInitialInput.getOrElse(Set()), tc._1.keys.toSet)
        }
      }
      assertEquals(numFormConstr, state.sygusData.formalConstr.size)
      assertEquals(tcInput, test.input)
    }

    val testVars = Some(Set("a", "b"))
    val benchs = List(
      ("resources/LIA/tests/max2_f_diffNames.sl", true, 0, 3, Map("a"->5, "b"->9), None),
      ("resources/LIA/tests/max2_f_reversedNames.sl", true, 0, 3, Map("y"->5, "x"->9), None),
      ("resources/LIA/tests/max2_f_sameNames.sl", true, 0, 3, Map("x"->5, "y"->9), None),
      ("resources/LIA/tests/max2_m.sl", false, 5, 2, Map("x"->5, "y"->9), testVars), // x,y because the spec is incomplete
      ("resources/LIA/tests/max2_t.sl", false, 5, 0, Map("x"->5, "y"->9), testVars),
      ("resources/LIA/tests/max2_t_spuriousVars.sl", false, 5, 0, Map("x"->5, "y"->9), testVars)
    )
    benchs.foreach{ b => testBenchmark(b._1, b._2, b._3, b._4, b._5, b._6) }
  }


  @Test
  def test_modelToSynthFunInputs(): Unit = {
    assertEquals(Map("a"->0, "b"->1), CDGPState.modelToSynthFunInputs(Map("x"->0, "y"->1), Seq("x", "y"), Seq("a", "b")))
    assertEquals(Map(), CDGPState.modelToSynthFunInputs(Map("x"->0, "y"->1), Seq(), Seq()))
    assertEquals(Map("a"->0, "b"->1), CDGPState.modelToSynthFunInputs(Map("x"->0, "y"->1, "z"->9), Seq("x", "y"), Seq("a", "b")))
    assertEquals(Map("a"->0, "b"->2, "c"->1), CDGPState.modelToSynthFunInputs(Map("x"->0, "y"->1), Seq("x", "2", "y"), Seq("a", "b", "c")))
    assertEquals(Map("a"->1, "b"->2, "c"->3), CDGPState.modelToSynthFunInputs(Map("x"->0, "y"->1), Seq("1", "2", "3"), Seq("a", "b", "c")))

  }
}
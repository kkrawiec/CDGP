package tests

import cdgp._
import org.junit.Test
import org.junit.Assert._


final class TestSygusUtils {
  @Test
  def testSygusParsing1(): Unit = {
    val code =
"""(set-logic LIA)
(synth-fun funSynth ((a Int) (b Int) (c Int)) Int ((Start Int (a b c))))
(declare-var c Int)
(declare-var b Int)
(declare-var a Int)
(constraint (=> (= (- b a) (- c b)) (= (funSynth a b c) 1)))
(constraint (=> (not (= (- b a) (- c b))) (= (funSynth a b c) 0)))
(check-synth)"""
    val problem = LoadSygusBenchmark.parseText(code)
    // println(problem.cmds.mkString("\n"))
    assertEquals(false, SygusUtils.containsUnsupportedTerms(problem))
    assertEquals(true, SygusUtils.hasSingleInvocationPropertyAllConstr(problem))
    assertEquals(Map("funSynth" -> List(List("a", "b", "c"), List("a", "b", "c"))),
      SygusUtils.getSynthFunsInvocationsInfo(SygusUtils.getAllConstraints(problem), Set("funSynth")))
  }

  @Test
  def testSygusParsing2(): Unit = {
    val code =
"""(set-logic LIA)
(synth-fun funSynth ((a Int) (b Int) (c Int)) Int ((Start Int (a b c))))
(declare-var c Int)
(declare-var b Int)
(declare-var a Int)
(constraint (=> (= (- b a) (- c b)) (= (funSynth a 1 c) 1)))
(constraint (=> (not (= (- b a) (- c b))) (= (funSynth a c 1) 0)))
(check-synth)"""
    val problem = LoadSygusBenchmark.parseText(code, checkSupport=false)
    assertEquals(false, SygusUtils.hasSingleInvocationPropertyAllConstr(problem))
    assertEquals(Map("funSynth" -> List(List("a", "1", "c"), List("a", "c", "1"))),
      SygusUtils.getSynthFunsInvocationsInfo(SygusUtils.getAllConstraints(problem), Set("funSynth")))
    assertEquals(false, SygusUtils.containsUnsupportedTerms(problem))
  }

  @Test
  def testSygusParsing3(): Unit = {
    val code =
"""(set-logic LIA)
(synth-fun funSynth ((a Int) (b Int) (c Int)) Int ((Start Int (a b c))))
(declare-var c Int)
(declare-var b Int)
(declare-var a Int)
(constraint (=> (= (- b a) (- c b)) (= (funSynth a 1 c) 1)))
(constraint (=> (not (= (- b a) (- c b))) (= (funSynth a c (funSynth a c 1)) 0)))
(check-synth)"""
    val problem = LoadSygusBenchmark.parseText(code)
    assertEquals(false, SygusUtils.hasSingleInvocationPropertyAllConstr(problem))
    assertEquals(true, SygusUtils.containsUnsupportedTerms(problem))
  }

  @Test
  def testSygusParsingLet1(): Unit = {
    val code =
      """(set-logic LIA)
(synth-fun funSynth ((a Int) (b Int) (c Int)) Int ((Start Int (a b c))))
(declare-var a Int)
(declare-var b Int)
(declare-var c Int)
(constraint (let ((m Int b)(b1 Int a)(b2 Int c))
    (=> (and (>= (abs b1) m) (>= (abs b2) m) ) (= (funSynth a b c) m))))
(check-synth)"""
    val problem = LoadSygusBenchmark.parseText(code)
    assertEquals(true, SygusUtils.hasSingleInvocationPropertyAllConstr(problem))
    assertEquals(false, SygusUtils.containsUnsupportedTerms(problem))
  }

  @Test
  def testSygusParsingLet2(): Unit = {
    val code =
      """(set-logic LIA)
(synth-fun funSynth ((a Int) (b Int) (c Int)) Int ((Start Int (a b c))))
(declare-var a Int)
(declare-var b Int)
(declare-var c Int)
(constraint (let ((m Int b)(b1 Int a)(b2 Int c))
    (=> (and (>= (abs b1) m) (>= (abs b2) m) ) (= (funSynth b1 m b2) m))))
(check-synth)"""
    val problem = LoadSygusBenchmark.parseText(code)
    assertEquals(true, SygusUtils.hasSingleInvocationPropertyAllConstr(problem))
    assertEquals(true, SygusUtils.containsUnsupportedTerms(problem))
  }

  @Test
  def testSygusDependencyMap(): Unit = {
    val code =
      """(set-logic LIA)
(synth-fun funSynth ((a Int) (b Int) (c Int)) Int ((Start Int (a b c))))
(declare-var a Int)
(declare-var b Int)
(declare-var c Int)
(define-fun x ((a Int)(b Int)) Int (+ a (let ((y Int 5)) (+ y c))))
(define-fun y ((a Int)) Int (x a b))"""
    val problem = LoadSygusBenchmark.parseText(code)
    val d = SygusUtils.getFunDefDependencyMap(problem)
    assertEquals(Map("x"->Set("c", "+"), "y"->Set("x", "b")), d)
  }

  @Test
  def testRecursiveSynthFun(): Unit = {
    val code =
      """(set-logic LIA)
(synth-fun funSynth ((a Int) (b Int) (c Int)) Int ((Start Int (a b c (funSynth Start Start Start)))))
      """
    val problem = LoadSygusBenchmark.parseText(code)
    val st = SygusSynthTask(problem).head
    assertEquals(true, st.canBeRecursive)
  }

  @Test
  def testRecursiveSynthFun2(): Unit = {
    val code =
      """(set-logic LIA)
        |(synth-fun recfun ((a Int)) Int
        |    ((Start Int (a 0
        |        (+ Start Start)
        |        (- Start Start)
        |        (ite StartBool Start Start)
        |        (recfun Start)))
        |     (StartBool Bool (
        |        (> Start Start)
        |        (< Start Start)
        |        (= Start Start)))
        |    )
        |)
        |(declare-var a Int)
        |(constraint (>= a 0))
        |(constraint (= (recfun 0) 0))
        |(constraint (= (recfun (+ a 1)) (+ 1 (recfun a)) ))
        |(check-synth)
      """.stripMargin
    val problem = LoadSygusBenchmark.parseText(code)
    val st = SygusSynthTask(problem).head
    assertEquals(true, st.canBeRecursive)
    assertEquals("(define-fun-rec recfun ((a Int)) Int (recfun 1))", st.getSynthFunCode("(recfun 1)"))
    assertEquals("(define-fun recfun ((a Int)) Int (recfun2 1))", st.getSynthFunCode("(recfun2 1)"))
    assertEquals("(define-fun recfun ((a Int)) Int (+ 1 1))", st.getSynthFunCode("(+ 1 1)"))
  }

  @Test
  def test_getPreconditions(): Unit = {
    val script =
      """
      (set-logic NIA)
      (synth-fun rsconf ((a Int) (b Int) (c Int) (d Int)) Int
          ((Start Int (a b c d
              (+ Start Start) (- Start Start) (* Start Start)))))
      (declare-var a Int)
      (declare-var b Int)
      (declare-var c Int)
      (declare-var d Int)
      (define-fun ElementsSum () Int 32)
      (constraint (>= a 0))
      (constraint (>= b 0))
      (constraint (>= c 0))
      (constraint (>= d 0))
      (constraint (= (+ a b c d) ElementsSum))

      (define-fun condition ((a Int)(b Int)(c Int)(d Int)) Int
          (- (* a ElementsSum) (* (+ a c) (+ a c)))
      )
      (constraint (=> (> (condition a b c d) 0)
                      (> (rsconf a b c d) 0)))
      (constraint (=> (< (condition a b c d) 0)
                      (< (rsconf a b c d) 0)))
      (constraint (=> (= (condition a b c d) 0)
                      (= (rsconf a b c d) 0)))
      (check-synth)
      """
    val problem = LoadSygusBenchmark.parseText(script)
    val precond = SygusUtils.getPreconditions(problem)
    assertEquals(Set("rsconf"), SygusUtils.getPostcondSymbols(problem))
    assertEquals(5, precond.size)
    assertEquals("(>= a 0)", SMTLIBFormatter.termToSmtlib(precond(0)))
    val st = SygusSynthTask(problem).head
    assertEquals(false, st.canBeRecursive)
  }

  @Test
  def test_getPreconditions2(): Unit = {
    val script =
      """
      (set-logic NIA)
      (synth-fun rsconf ((a Int) (b Int) (c Int) (d Int)) Int
          ((Start Int (a b c d
              (+ Start Start) (- Start Start) (* Start Start)))))
      (declare-var a Int)
      (declare-var b Int)
      (declare-var c Int)
      (declare-var d Int)
      (define-fun ElementsSum () Int 32)
      (precondition (>= a 0))
      (precondition (>= b 0))
      (precondition (>= c 0))
      (precondition (>= d 0))
      (precondition (= (+ a b c d) ElementsSum))

      (define-fun condition ((a Int)(b Int)(c Int)(d Int)) Int
          (- (* a ElementsSum) (* (+ a c) (+ a c)))
      )
      (constraint (=> (> (condition a b c d) 0)
                      (> (rsconf a b c d) 0)))
      (constraint (=> (< (condition a b c d) 0)
                      (< (rsconf a b c d) 0)))
      (constraint (=> (= (condition a b c d) 0)
                      (= (rsconf a b c d) 0)))
      (check-synth)
      """
    val problem = LoadSygusBenchmark.parseText(script)
    val precond = SygusUtils.getPreconditions(problem)
    assertEquals(Set("rsconf"), SygusUtils.getPostcondSymbols(problem))
    assertEquals(5, precond.size)
    assertEquals("(>= a 0)", SMTLIBFormatter.termToSmtlib(precond(0)))
    val st = SygusSynthTask(problem).head
    assertEquals(false, st.canBeRecursive)
  }

  @Test
  def test_collectFreeVars1(): Unit = {
    val code = """(set-logic LIA)
(synth-fun funSynth ((a Int) (b Int) (c Int)) Int ((Start Int (a b c))))
(declare-var d Int)
(declare-var c Int)
(declare-var b Int)
(declare-var a Int)
(constraint (> d 0))
(constraint (=> (= (- b a) (- c b)) (= (funSynth a b c) 1)))
(constraint (=> (not (= (- b a) (- c b))) (= (funSynth a b c) 0)))
(check-synth)"""
    val problem = LoadSygusBenchmark.parseText(code)
    val precond = SygusUtils.getPreconditions(problem)
    val postcond = SygusUtils.getPostconditions(problem)
    assertEquals(Set("a", "b", "c"), SygusUtils.collectFreeVars(postcond))
    assertEquals(Set("d"), SygusUtils.collectFreeVars(precond))
  }

  @Test
  def test_collectFreeVars2(): Unit = {
    val code = """(set-logic LIA)
(synth-fun funSynth ((a Int) (b Int) (c Int)) Int ((Start Int (a b c))))
(declare-fun fun_d () Int)
(define-fun fun_e () Int 5)
(declare-var c Int)
(declare-var b Int)
(declare-var a Int)
(constraint (=> (= (- b a) (- c b)) (= (funSynth a fun_e fun_d) 1)))
(constraint (=> (not (= (- b a) (- c b))) (= (funSynth a fun_e fun_d) 0)))
(check-synth)"""
    val problem = LoadSygusBenchmark.parseText(code)
    val precond = SygusUtils.getPreconditions(problem)
    val postcond = SygusUtils.getPostconditions(problem)
    assertEquals(Set("a", "b", "c", "fun_d", "fun_e"), SygusUtils.collectFreeVars(postcond))
    assertEquals(Set(), SygusUtils.collectFreeVars(precond))
  }

  @Test
  def test_collectConstants1(): Unit = {
    val code = """(set-logic LIA)
(synth-fun funSynth ((a Int) (b Int) (c Int)) Int ((Start Int (a b c))))
(declare-var d Int)
(declare-var c Int)
(declare-var b Int)
(declare-var a Int)
(constraint (> d 0))
(constraint (=> (= (- b 1.0) (- c "asd")) (= (funSynth a b c) 1)))
(constraint (=> (not (= (- 100 a) (- c b))) (= (funSynth a b c) 0)))
(check-synth)"""
    val problem = LoadSygusBenchmark.parseText(code)
    val precond = SygusUtils.getPreconditions(problem)
    val postcond = SygusUtils.getPostconditions(problem)
    assertEquals(Set(1.0, "asd", 100, 1, 0), SygusUtils.collectConstants(postcond))
    assertEquals(Set(0), SygusUtils.collectConstants(precond))
  }

  @Test
  def test_forallExists(): Unit = {
    val code = """(set-logic LIA)
(synth-fun funSynth ((a Int)(b Int)) Int ((Start Int (a b))))
(declare-var a Int)
(constraint (forall ((i Int)) (>= (funSynth a 1) a)))
(constraint (exists ((i Int)) (= (funSynth a i) 0)))
(check-synth)"""
    val problem = LoadSygusBenchmark.parseText(code)
    val postcond = SygusUtils.getPostconditions(problem)
    assertEquals("(forall ((i Int)) (>= (funSynth a 1) a))", SMTLIBFormatter.termToSmtlib(postcond(0).t))
    assertEquals("(exists ((i Int)) (= (funSynth a i) 0))", SMTLIBFormatter.termToSmtlib(postcond(1).t))
  }

  @Test
  def test_getPostcondSymbols(): Unit = {
    assertEquals(Set("a", "b", "f2", "f1"), SygusUtils.getPostcondSymbols(
      Set("a", "b"), Map("f1"->Set("x", "y", "f2"), "f2"->Set("a", "x", "+"))))
    assertEquals(Set("a", "b"), SygusUtils.getPostcondSymbols(
      Set("a", "b"), Map("f1"->Set("x", "y"))))
  }

  @Test
  def test_renameVars1(): Unit = {
    val testInputsMap = Map("x" -> -54)
    val synFunArgNames = List("x", "y")
    val invNames = List("x")
    assertEquals(Map("x" -> -54), SygusUtils.renameVars(testInputsMap, invNames, synFunArgNames))
  }

  @Test
  def test_renameVars2(): Unit = {
    val testInputsMap = Map("a" -> -54, "b" -> 2)
    val synFunArgNames = List("x", "y")
    val invNames = List("a", "300")
    assertEquals(Map("x" -> -54, "y" -> 300), SygusUtils.renameVars(testInputsMap, invNames, synFunArgNames))
  }
}
package tests

import cdgp.{LoadSygusBenchmark, UnsupportedFeatureException}
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
    assertEquals(true, LoadSygusBenchmark.hasSingleInvocationProperty(problem))
    assertEquals(Map("funSynth" -> List(List("a", "b", "c"), List("a", "b", "c"))),
                 LoadSygusBenchmark.getSynthFunsInvocationsInfo(problem, Set("funSynth")))
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
    assertEquals(false, LoadSygusBenchmark.hasSingleInvocationProperty(problem))
    assertEquals(Map("funSynth" -> List(List("a", "1", "c"), List("a", "c", "1"))),
                 LoadSygusBenchmark.getSynthFunsInvocationsInfo(problem, Set("funSynth")))
  }

  @Test(expected=classOf[UnsupportedFeatureException])
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
    val problem = LoadSygusBenchmark.parseText(code)  // this should throw exception
  }
}
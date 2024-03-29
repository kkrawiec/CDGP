package tests

import cdgp._
import fuel.util.Options
import org.junit.Test
import org.junit.Assert._
import swim.tree.Op

class TestTools {
  @Test
  def test_duplicateElements(): Unit = {
    assertEquals(List(), Tools.duplicateElements(List(), 2))
    assertEquals(List(1, 1), Tools.duplicateElements(List(1), 2))
    assertEquals(List(1, 1, 1, 2, 2, 2), Tools.duplicateElements(List(1, 2), 3))
    assertEquals(List(1, 2), Tools.duplicateElements(List(1, 2), 1))
  }

  @Test
  def test_allOccurences(): Unit = {
    assertEquals(List(0, 8), Tools.allOccurences("asd fgh asd", "a"))
    assertEquals(List(), Tools.allOccurences("asd fgh asd", "z"))
    assertEquals(List(0, 1, 2), Tools.allOccurences("aaa", "a"))
  }

  @Test
  def test_convertHexToChars(): Unit = {
    assertEquals("asd fgh", Tools.convertSmtToJavaString("asd fgh"))
    assertEquals(" A ", Tools.convertSmtToJavaString(" \\x41 "))
    assertEquals("\n", Tools.convertSmtToJavaString("\\n"))
    assertEquals("as\nas", Tools.convertSmtToJavaString("as\\nas"))
    assertEquals("\"", Tools.convertSmtToJavaString("\"\""))
    assertEquals(Character.toString(7) + "\b", Tools.convertSmtToJavaString("\\a\\b"))
    assertEquals(Character.toString(11), Tools.convertSmtToJavaString("\\v"))
  }

  @Test
  def test_splitTrainValidationTest1(): Unit = {
    val tests = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val (train1, valid1, test1) = Tools.splitTrainValidationTest(tests)(Options("--sizeTrainSet 4"))
    assertEquals((4, 0, 0), (train1.size, valid1.size, test1.size))
    val (train2, valid2, test2) = Tools.splitTrainValidationTest(tests)(Options("--sizeTestSet 4"))
    assertEquals((6, 0, 4), (train2.size, valid2.size, test2.size))
    val (train3, valid3, test3) = Tools.splitTrainValidationTest(tests)(Options("--sizeTestSet 20%"))
    assertEquals((8, 0, 2), (train3.size, valid3.size, test3.size))
    val (train4, valid4, test4) = Tools.splitTrainValidationTest(tests)(Options("--sizeTestSet 25.8%"))
    assertEquals((8, 0, 2), (train4.size, valid4.size, test4.size))
  }

  @Test
  def test_splitTrainValidationTest2(): Unit = {
    val tests = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val (train1, val1, test1) = Tools.splitTrainValidationTest(tests)(Options("--sizeTrainSet 6 --sizeValidationSet 2 --sizeTestSet 2"))
    assertEquals((6, 2, 2), (train1.size, val1.size, test1.size))
    val (train2, val2, test2) = Tools.splitTrainValidationTest(tests)(Options("--sizeTestSet 4"))
    assertEquals((6, 0, 4), (train2.size, val2.size, test2.size))
    val (train3, val3, test3) = Tools.splitTrainValidationTest(tests)(Options("--sizeValidationSet 30%"))
    assertEquals((7, 3, 0), (train3.size, val3.size, test3.size))
  }

  @Test
  def test_stringScientificNotation(): Unit = {
    assertEquals("1E-3,1E2", Tools.stringScientificNotation(Seq(0.001, 100)))
  }

  @Test
  def test_getOpDividers(): Unit = {
    val divs1 = Tools.getOpDividers(Op.fromStr("/(x 12)", useSymbols=false)).toSet
    val divs2 = Tools.getOpDividers(Op.fromStr("/(x /(x 12))", useSymbols=false)).toSet
    val divs3 = Tools.getOpDividers(Op.fromStr("+(/(x 15) /(x 12))", useSymbols=false)).toSet
    val divs4 = Tools.getOpDividers(Op.fromStr("+(/(x 15) /(x 12))", useSymbols=true)).toSet
    assertEquals(Set("12"), divs1.map(_.toString))
    assertEquals(Set("12", "/(x 12)"), divs2.map(_.toString))
    assertEquals(Set("15", "12"), divs3.map(_.toString))
    assertEquals(Set("15", "12"), divs4.map(_.toString))
  }
}

package tests

import cdgp._
import org.junit.Test
import org.junit.Assert._

class TestTools {
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
}

package tests

import org.junit.Test
import org.junit.Assert._
import cdgp.GetValueParser


class TestGetValueParser {

  @Test
  def testPass(): Unit = {
    assertEquals( Map( "x" -> 1, "y" -> -2 ), GetValueParser("""((x 1)
               (y (- 2)))""").toMap)

    assertEquals( Map( "x" -> -7787, "y" -> 0 ), GetValueParser(""" ((x (- 7787))
               (y (- 0)))""").toMap)

    assertEquals( Map( "x" -> -7787, "y" -> -2 ), GetValueParser(""" ((x -7787)
               (y -2))""").toMap)
  }

  @Test(expected=classOf[GetValueParser.ValueParseException])
  def testFail(): Unit = {
    GetValueParser(""" ((x (- 7787))
      (y (- ERROR)))""")
  }
}

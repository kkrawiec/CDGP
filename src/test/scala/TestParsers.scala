package tests

import org.junit.Test
import org.junit.Assert._
import cdgp.{ConstParser, GetValueParser, ValueParseException}


class TestGetValueParser {

  @Test
  def testPass(): Unit = {
    assertEquals( Map( "x" -> 1, "y" -> -2 ), GetValueParser("""((x 1)
               (y (- 2)))""").toMap)

    assertEquals( Map( "x" -> -7787, "y" -> 0 ), GetValueParser(""" ((x (- 7787))
               (y (- 0)))""").toMap)

    assertEquals( Map( "x" -> -7787, "y" -> -2 ), GetValueParser(""" ((x -7787)
               (y -2))""").toMap)

    assertEquals( Map( "x" -> "cdgp", "y" -> -2 ), GetValueParser(""" ((x "cdgp")
               (y -2))""").toMap)

    assertEquals( Map( "x" -> "cdgp", "y" -> -2, "z" -> "eps" ), GetValueParser(""" ((x "cdgp")
               (y -2)(z "eps"))""").toMap)
  }

  @Test
  def testGetValueTooBigInt(): Unit = {
    try {
      GetValueParser("""((x 12345678901234)(y (- 2)))""").toMap
      fail()
    } catch { case e: Throwable => }
  }

  @Test
  def testParserString(): Unit = {
    val model = """((s "") (a 0) (b 0) (res1__2 "AAAAAAAAAA") (res2__2 "BAAAAAAAAA"))"""
    val parsed = GetValueParser(model).toMap
    assertEquals(Map("s" -> "", "a" -> 0, "b" -> 0, "res1__2" -> "AAAAAAAAAA", "res2__2" -> "BAAAAAAAAA"),
      parsed)
  }


  @Test(expected=classOf[ValueParseException])
  def testFail(): Unit = {
    GetValueParser(""" ((x (- 7787))
      (y (- ERROR)))""")
  }

  @Test
  def testComplex(): Unit = {
    val res = GetValueParser("""((a 15) (b 14) (c 3) (d 0) (res1__2a (- 16)) (res2__2a (- 32)))""")
    assertEquals(Map("a"->15, "b"->14, "c"->3, "d"->0, "res1__2a"->(-16), "res2__2a"->(-32)), res.toMap)
  }
}



class TestConstParser {

  @Test
  def testPass(): Unit = {
    assertEquals(true, ConstParser("true"))
    assertEquals(false, ConstParser("false"))

    assertEquals("asd", ConstParser("\"asd\""))
    assertEquals("", ConstParser("\"\""))

    assertEquals(123, ConstParser("123"))
    assertEquals(-4, ConstParser("-4"))
    assertEquals(-4, ConstParser("(- 4)"))

    assertEquals(123.0, ConstParser("123.0"))
    assertEquals(1.25, ConstParser("1.25"))
    assertEquals(1.25, ConstParser("+1.25"))
    assertEquals(-4.5, ConstParser("-4.5"))
    assertEquals(1e-3, ConstParser("1e-3"))
    assertEquals(-1e-3, ConstParser("-1e-3"))
  }

  @Test(expected=classOf[ValueParseException])
  def testFail(): Unit = {
    ConstParser("[1, 2, 3]")
  }
}
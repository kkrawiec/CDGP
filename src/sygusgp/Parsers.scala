package sygusgp

import scala.util.parsing.combinator.RegexParsers

/* Parses the output of the solver's get-value command, 
 * which returns the model as a list of pairs (variable, value)
 * TODO: Extend with other types (for now only Int)
 */

object GetValueParser extends RegexParsers {
  
  case class ValueParseException(msg: String) extends RuntimeException(msg)
  
  /////////////////////////////////
  
  val varName: Parser[String] = """[A-Za-z]+[0-9]*""".r
  
  def posNumber: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def negNumber: Parser[Int] = "(-" ~> number ~ ")" ^^ {
    case a ~ _ => -a
  }
  def number: Parser[Int] = posNumber | negNumber
  
  def entry: Parser[(String, Int)] = "(" ~> varName ~ number <~ ")" ^^ {
    case a ~ b => (a, b.toInt)
  }
  
  def values: Parser[List[(String, Int)]] = "(" ~> rep1(entry) ~ ")" ^^ {
    case list ~ _ => list
  }
  
  def apply(s: String) = GetValueParser.parse(GetValueParser.values, s) match {
    case m @ Failure(msg, next) => throw ValueParseException(s"Parse failure: $msg")
    case m @ Error(msg, next)   => throw ValueParseException(s"Parse error: $msg")
    case Success(result, next)  => result
  }
}

///////////////////////////////////

import org.junit._
import org.junit.Assert._

class TestGetValueParser {

  @Test
  def testPass: Unit = {
    
    assertEquals( Map( "x" -> 1, "y" -> -2 ), GetValueParser(""" ((x 1)
               (y (- 2)))""") )

    assertEquals( Map( "x" -> - 7787, "y" -> -2 ), GetValueParser(""" ((x (- 7787))
               (y (- 2)))"""))
  }
  
  @Test(expected=classOf[GetValueParser.ValueParseException])
  def testFail: Unit = {
    GetValueParser(""" ((x (- 7787))
      (y (- ERROR)))""")    
  }
}

// End ///////////////////////////////////////////////////////////////

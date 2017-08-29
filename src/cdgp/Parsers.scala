package cdgp

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers


case class ValueParseException(msg: String) extends RuntimeException(msg)


/* Parses the output of the solver's get-value command, 
 * which returns the model as a list of pairs (variable, value)
 * TODO: Extend with other types (for now only Int)
 */
object GetValueParser extends RegexParsers {
  val varName: Parser[String] = """[A-Za-z]+[0-9]*""".r
  
  def posNumber: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def negNumber: Parser[Int] = "(-" ~> number ~ ")" ^^ { case a ~ _ => -a }
  def negNumber2: Parser[Int] = "-" ~> number ^^ { a => -a }
  def number: Parser[Int] = posNumber | negNumber | negNumber2
  
  def entry: Parser[(String, Int)] = "(" ~> varName ~ number <~ ")" ^^ {
    case a ~ b => (a, b.toInt)
  }
  
  def values: Parser[List[(String, Int)]] = "(" ~> rep1(entry) ~ ")" ^^ {
    case list ~ _ => list
  }
  
  def apply(s: String): List[(String, Int)] = GetValueParser.parse(GetValueParser.values, s) match {
    case m @ Failure(msg, next) => throw ValueParseException(s"Parse failure: $msg")
    case m @ Error(msg, next)   => throw ValueParseException(s"Parse error: $msg")
    case Success(result, next)  => result
  }
}


object ConstParser {
  def apply(s: String): Any = {
    val intPattern = raw"([0-9]+|-[0-9]+)"
    val doublePattern = "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"
    if (s == "true") true
    else if (s == "false") false
    else if (s.head == '\"' && s.last == '\"') s.substring(1, s.size-1)
    else if (s.matches(intPattern)) s.toInt
    else if (s.matches(raw"\(-\s[0-9]+\)")) -s.substring(3, s.size-1).toInt
    else if (s.matches(doublePattern)) s.toDouble
    else throw ValueParseException(f"Trying to parse unknown constant: $s")
  }
}

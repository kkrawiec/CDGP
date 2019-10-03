package cdgp

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers


case class ValueParseException(msg: String) extends RuntimeException(msg)


/*
 * Parses the output of the solver's get-value command, which returns the model
 * as a list of pairs (variable, value).
 */
object GetValueParser extends RegexParsers {
  val varName: Parser[String] = """[A-Za-z_\.-|][A-Za-z0-9_\.-|]*""".r
  val floatingPointRegex: Regex = "[+-]?[0-9]+[.][0-9]*".r
  val boolean: Parser[Boolean] = "true" ^^^ { true } | "false" ^^^ { false }

  def boolConst: Parser[Boolean] = boolean ^^ { b => b }
  def realConst: Parser[Double] =
    floatingPointRegex <~ opt("?") ^^ { x => x.toDouble } |
    ("(" ~ "-" ~> floatingPointRegex <~ opt("?") <~ ")" ^^ { x => -x.toDouble }) |
    ("(" ~ "/" ~> realConst ~ realConst <~ ")" ^^ { case x ~ y => x / y }) |
    ("(" ~ "/" ~> intConst ~ intConst <~ ")" ^^ { case x ~ y => x.toDouble / y })

  def posNumber: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def negNumber: Parser[Int] = (("(-" ~> intConst <~ ")") | ("-" ~> intConst)) ^^ { a => -a }
  def intConst: Parser[Int] = posNumber | negNumber
  def string: Parser[String] = """\"([^"]*)\"""".r ^^ { s =>  s.substring(1, s.size-1) }

  def entryAssigned: Parser[Any] = boolConst | realConst | intConst | string
  def entry: Parser[(String, Any)] = "(" ~> varName ~ entryAssigned <~ ")" ^^ { case a ~ b => (a, b) }

  def values: Parser[List[(String, Any)]] = "(" ~> rep1(entry) ~ ")" ^^ {
    case list ~ _ => list
  }
  
  def apply(s: String): List[(String, Any)] = GetValueParser.parse(GetValueParser.values, s) match {
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
    else if (s.head == '\"' && s.last == '\"') s.substring(1, s.size-1)  // String
    else if (s.matches(intPattern)) s.toInt
    else if (s.matches(raw"\(-\s[0-9]+\)")) -s.substring(3, s.size-1).toInt
    else if (s.matches(doublePattern)) s.toDouble
    else throw ValueParseException(s"Trying to parse unknown constant: $s")
  }
}

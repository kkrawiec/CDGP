package cdgp.solvers

import cdgp.ValueParseException

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Dreal3 {

  def verify(): Unit = {

  }

  /**
    * Parses output of the dReal3 solver.
    *
    * @param s Output of the dReal3 solver.
    * @return Decision and, if sat, a model assigning to each variable a bound on its values.
    */
  def parseResult(s: String): (Boolean, Option[Map[String, (Double, Double)]]) = {
    ???
  }

}


/**
  * Output for: ./dReal3 --model file-sat.smt2
  * Solution:
  * x : [ ENTIRE ] = [-INFTY, -INFTY]
  * y : [ ENTIRE ] = [-INFTY, -INFTY]
  * delta-sat with delta = 0.00100000000000000
  *
  * Output for: ./dReal3 --model file-sat2.smt2
  * Solution:
  * x : [ ENTIRE ] = [-2, -2]
  * delta-sat with delta = 0.00100000000000000
  *
  * Output for: ./dReal3 --model file-unsat.smt2
  * unsat
  *
  * Output for: ./dReal3 file-sat.smt2
  * delta-sat with delta = 0.00100000000000000
  *
  * Output for: ./dReal3 file-unsat.smt2
  * unsat
  */
object OutputParserDreal3 extends RegexParsers {
  val unguardedSymbolRegex: Regex = """[a-zA-Z\-[_\+\*&\|\!~<>=/%\?\.\$\^]]([a-zA-Z0-9\-[_\+\*&\|\!~<>=/%\?\.\$\^]])*""".r
  val guardedSymbolRegex: Regex = """[^|]+""".r
  val guardedSymbol: Parser[String] = "|" ~ guardedSymbolRegex ~ "|" ^^ { case _ ~ s ~ _ => s"|$s|" }
  val symbol: Parser[String] = guardedSymbol | unguardedSymbolRegex
  val double: Regex = "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?".r

  def posNumber: Parser[Double] = double ^^ { _.toDouble }
  def number: Parser[Double] =
    "INFTY" ^^^ { Double.PositiveInfinity } |
    "-INFTY" ^^^ { Double.NegativeInfinity } |
    posNumber

  def assignmentRange: Parser[(Double, Double)] =
    number ~ "," ~ number ^^ { case min ~ _ ~ max => (min, max) }

  def assignment: Parser[(String, (Double, Double))] =
    symbol ~ (":" ~ "[") ~ symbol ~ ("]" ~ "=" ~ "[") ~ assignmentRange <~ "]" ^^ {
      case sym ~ _ ~ rangeName ~ _ ~ range => (sym, range)
    }

  def satModel: Parser[(String, Option[Map[String, (Double, Double)]])] =
    "Solution:" ~> rep1(assignment) ~ satPlain ^^ { case list ~ _ =>
      ("sat", Some(list.toMap))
    }

  def satPlain: Parser[(String, Option[Map[String, (Double, Double)]])] =
    "delta-sat" ^^^ { ("sat", None) }

  def unsat: Parser[(String, Option[Map[String, (Double, Double)]])] =
    "unsat" ^^^ { ("unsat", None) }

  def main: Parser[(String, Option[Map[String, (Double, Double)]])] =
    unsat | satPlain | satModel

  def apply(s: String): (String, Option[Map[String, (Double, Double)]]) =
    OutputParserDreal3.parse(OutputParserDreal3.main, s) match {
      case m @ Failure(msg, next) => throw ValueParseException(s"Parse failure: $msg")
      case m @ Error(msg, next)   => throw ValueParseException(s"Parse error: $msg")
      case Success(result, next)  => result
  }
}
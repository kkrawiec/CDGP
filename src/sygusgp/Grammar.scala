package sygusgp

import scala.collection.immutable.Seq

import sygus.BVConst
import sygus.BoolConst
import sygus.BoolSortExpr
import sygus.CompositeGTerm
import sygus.GTerm
import sygus.GenericGTerm
import sygus.IntConst
import sygus.IntSortExpr
import sygus.LetGTerm
import sygus.Literal
import sygus.LiteralGTerm
import sygus.NTDef
import sygus.RealConst
import sygus.SortExpr
import sygus.StringConst
import sygus.SymbolGTerm
import sygus.SynthFunCmd14
import sygus.SynthFunCmd16
import sygus16.SyGuS16

object ExtractSynthesisTasks {
  def apply(tree: SyGuS16) = tree.cmds.collect {
    case SynthFunCmd14(sym: String, args: List[(String, SortExpr)], se: SortExpr, ntDefs: List[NTDef]) => {
      val grammar = retrieveGrammar(ntDefs)
      (sym, grammar, args, se) // name, function syntax, args list, output type
    }
    case SynthFunCmd16(sym: String, args: List[(String, SortExpr)], se: SortExpr) => {
      // Add the variables 
      val bp = boolProd(args.filter(_._2 == BoolSortExpr()).map(_._1.toString))
      val ip = intProd(args.filter(_._2 == IntSortExpr()).map(_._1.toString))
      // The first symbol in the grammar is the initial symbol, and that symbol depends
      // on the output type of the function:
      val grammar = se match {
        case BoolSortExpr() => List(bp, ip)
        case IntSortExpr()  => List(ip, bp)
      }
      (sym, grammar, args, se)
    }
  } 

  // Default grammar for the language of entire LIA (called 'Conditional Linear Integer
  // arithmetic' in SygusComp16.pdf)
  // Constants are fixed for now: 
  def intProd(vars: Seq[Any]): (Any, Seq[Any]) = 'I -> (vars ++ Seq(
    -1, 0, 1,
    "+" -> ('I, 'I),
    "-" -> ('I, 'I),
    "ite" -> ('B, 'I, 'I)))
  def boolProd(vars: Seq[Any]): (Any, Seq[Any]) = 'B -> (vars ++ Seq(
    true, false,
    "=" -> ('I, 'I),
    "<" -> ('I, 'I),
    "<=" -> ('I, 'I),
    ">" -> ('I, 'I),
    ">=" -> ('I, 'I),
    "and" -> ('B, 'B),
    "or" -> ('B, 'B),
    "not" -> ('B)))

  def retrieveGrammar(ntDefs: List[NTDef]): List[(Any, Seq[Any])] = ntDefs.map {
    case NTDef(symbol: String, sortExpr: SortExpr, gterms: List[GTerm]) =>
      symbol -> {
        gterms.map({
          case CompositeGTerm(symbol: String, terms: List[GTerm]) => symbol -> terms.map {
            case CompositeGTerm(symbol: String, terms: List[GTerm])           => symbol
            case LiteralGTerm(literal: Literal)                               => literal
            case SymbolGTerm(symbol: String)                                  => symbol //Input(argNames.indexOf(symbol))
            case LetGTerm(list: List[(String, SortExpr, GTerm)], term: GTerm) => 0 // TODO
          }
          case LiteralGTerm(literal: Literal) => literal match {
            case IntConst(value: Int)          => value
            case RealConst(value: Double)      => value
            case BoolConst(value: Boolean)     => value
            case BVConst(value: List[Boolean]) => value
            case StringConst(value: String)    => value
          }
          case SymbolGTerm(symbol: String)                                  => symbol
          case LetGTerm(list: List[(String, SortExpr, GTerm)], term: GTerm) => 0 // TODO: Not implemented yet
          case GenericGTerm(identifier: String, sortExpr: SortExpr)         => 0 // TODO
        }).toSeq
      }
  }
}

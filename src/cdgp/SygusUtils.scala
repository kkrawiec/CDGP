package cdgp

import java.io.File

import swim.Grammar

import scala.collection.immutable.Seq
import sygus._
import sygus16.SyGuS16



class UnsupportedFeatureException(message: String = "", cause: Throwable = null)
  extends Exception(message, cause)



/**
  * Class collecting the most important information about the synthesis task
  * read from the SyGuS file.
  * @param fname Name of the function being synthesized.
  * @param grammar Grammar specifying the form of allowed programs.
  * @param arguments Arguments of the function.
  * @param outputType Output type of the function.
  */
case class SygusSynthesisTask(fname: String,
                              grammar: Seq[(Any, Seq[Any])],
                              arguments: Seq[(String, SortExpr)],
                              outputType: SortExpr) {
  val argNames: Seq[String] = arguments.unzip._1
}


object LoadSygusBenchmark {
  def apply(path: String, checkSupport: Boolean = true): SyGuS16 = {
    parseText(loadBenchmarkContent(path), checkSupport)
  }

  def parseText(code: String, checkSupport: Boolean = true): SyGuS16 = {
    val parseRes = SyGuS16.parseSyGuS16Text(code)
    if (parseRes.isLeft)
      throw new Exception("PARSE ERROR: " + parseRes.left)
    assume(parseRes.isRight)
    val res = parseRes match { case Right(t) => t }
    if (checkSupport)
      checkIfSupported(res)
    res
  }

  /**
    * Checks, if the loaded problem can be solved by CDGP and if not throws
    * UnsupportedFeatureException with appropriate message.
    */
  def checkIfSupported(problem: SyGuS16) {
    if (!hasSingleInvocationProperty(problem))
      throw new UnsupportedFeatureException("CDGP supports only problems with the single invocation property.")
    checkUnsupportedTerms(problem)
  }

  def checkUnsupportedTerms(problem: SyGuS16) {
    val synthFunNames = ExtractSynthesisTasks(problem).map(_.fname).toSet
    def checkExpr(term: Term): Unit = term match {
      case LetTerm(_, _) => throw new UnsupportedFeatureException("Let terms are not supported.")
      case CompositeTerm(symbol, terms) if synthFunNames.contains(symbol) =>
        terms.foreach {
          case CompositeTerm(_, _) => throw new UnsupportedFeatureException("Invocation of a synthesized function must take as an argument a literal or a variable.")
          case _ => terms.foreach{ x: Term => checkExpr(x) }
        }
      case c: CompositeTerm =>
        c.terms.foreach{ x: Term => checkExpr(x) }
      case _ => ()
    }
    problem.cmds.foreach{ case ConstraintCmd(term) => checkExpr(term) case _ => () }
  }

  /**
    * Checks, if a set of constraints has single invocation property. This property
    * states that if there is a function f, then every invocation of this function
    * in the constraints takes exactly the same arguments.
    *
    * Consider function f(x,y). The following constraint has the property:
    * f(x,y) >= x && f(x,y) >= y
    * while this one does not:
    * f(x,y) == f(y,x)
    */
  def hasSingleInvocationProperty(problem: SyGuS16): Boolean = {
    val sfs = ExtractSynthesisTasks(problem)
    val setNames = sfs.map(_.fname).toSet
    val invInfo = getSynthFunsInvocationsInfo(problem, setNames)
    invInfo.forall{ case (n, lst) => lst.toSet.size == 1}
  }

  /**
    * Creates a map assigning to each provided synth function a list of arguments
    * this function was invoked with in the constraints. Each invocation is represented
    * as a distinct entry, so there may be duplicates.
    */
  def getSynthFunsInvocationsInfo(problem: SyGuS16, setNames: Set[String]): Map[String, Seq[Seq[String]]] = {
    def searchExpr(term: Term): List[(String, List[String])] = term match {
      case c: CompositeTerm if setNames.contains(c.symbol) =>
        val tup = (c.symbol, c.terms.map{
          case LiteralTerm(v) => v.toString
          case SymbolTerm(s) => s
          case x @ CompositeTerm(_, _) => x.toString
        })
        List(tup)
      case c: CompositeTerm =>
        c.terms.flatMap{ x: Term => searchExpr(x) }
      case _ => List()
    }
    val collected: Seq[(String, List[String])] = problem.cmds.collect {
        case ConstraintCmd(term) => searchExpr(term)
      }.flatten
    val gr = collected.groupBy(_._1).map{ case (k, v) => (k, v.map(_._2)) }
    println("gr: " + gr)
    gr
  }

  private def loadBenchmarkContent(benchmark: String): String = {
    try {
      scala.io.Source.fromFile(new File(benchmark)).mkString
    }
    catch {
      case _: java.io.FileNotFoundException =>
        throw new Exception(s"File with benchmark not found: $benchmark")
      case e: Throwable =>
        throw e
    }
  }
}


object ExtractSygusGrammar {
  def apply(synthTask: SygusSynthesisTask): Grammar = {
    val argNames = synthTask.argNames
    val grammarMap = synthTask.grammar.toMap
    val start = if (!grammarMap.contains("Start")) synthTask.grammar.head._1 else "Start"
    Grammar.fromMap(start, grammarMap)
  }
}


object ExtractSynthesisTasks {
  def apply(tree: SyGuS16): List[SygusSynthesisTask] = tree.cmds.collect {
    case SynthFunCmd14(sym: String, args: List[(String, SortExpr)], se: SortExpr, ntDefs: List[NTDef]) => {
      val grammar = retrieveGrammar(ntDefs)
      SygusSynthesisTask(sym, grammar, args, se) // name, function syntax, args list, output type
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
      SygusSynthesisTask(sym, grammar, args, se)
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

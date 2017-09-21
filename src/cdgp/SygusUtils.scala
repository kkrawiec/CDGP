package cdgp

import java.io.File

import swim.Grammar
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



object SygusUtils {

  /**
    * Solver requires renaming of the variables, so that it is able to handle cases
    * similar to the one below. This renaming is only needed for the execution of the
    * program by the domain.
    *   (synth-fun synthFun ((x Int)(y Int)) ... )
    *   (declare-var a Int)
    *   (declare-var b Int)
    *   (constraint (=> (= (- b a) (- c b)) (= (synthFun a b) 1)))
    */
  def renameVars(testInputsMap: Map[String, Any], synFunArgNames: Seq[String],
                 inv: Seq[String]): Map[String, Any] = {
    inv.zip(synFunArgNames).map{
      case (invName, stName) =>  // invName potentially may be a constant
        (stName, testInputsMap.getOrElse(invName, ConstParser(invName)))
    }.toMap
  }

  /**
    * Changes names of *variable* terms present in the command.
    * The map contains mapping between old and new names and does not have to be complete.
    */
  def renameVarsInCmd(cmd: Cmd, map: Map[String, String]): Cmd = {
    cmd match {
      case ConstraintCmd(term)  => ConstraintCmd(renameVarsInTerm(term, map))
      case VarDeclCmd(symb, se) => VarDeclCmd(map.getOrElse(symb, symb), se)
      case x => x
    }
  }

  /**
    * Changes names of *all* terms present in the command.
    * The map contains mapping between old and new names and does not have to be complete.
    */
  def renameNamesInCmd(cmd: Cmd, map: Map[String, String]): Cmd = {
    cmd match {
      case ConstraintCmd(term)  => ConstraintCmd(renameNamesInTerm(term, map))
      case VarDeclCmd(symb, se) => VarDeclCmd(map.getOrElse(symb, symb), se)
      case x => x
    }
  }

  /**
    * Changes names of *variable* terms in the expression.
    * The map contains mapping between old and new names and does not have to be complete.
    */
  def renameVarsInTerm(term: Term, map: Map[String, String]): Term = {
    term match {
      case CompositeTerm(name, terms) => CompositeTerm(name, terms.map(renameVarsInTerm(_, map)))
      case SymbolTerm(symb) => SymbolTerm(map.getOrElse(symb, symb))
      case x => x
    }
  }

  /**
    * Changes names of *all* terms in the expression, including names of functions.
    * The map contains mapping between old and new names and does not have to be complete.
    */
  def renameNamesInTerm(term: Term, map: Map[String, String]): Term = {
    term match {
      case CompositeTerm(name, terms) =>
        CompositeTerm(map.getOrElse(name, name), terms.map(renameNamesInTerm(_, map)))
      case SymbolTerm(symb) => SymbolTerm(map.getOrElse(symb, symb))
      case x => x
    }
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
    collected.groupBy(_._1).map{ case (k, v) => (k, v.map(_._2)) }
  }

  def getSynthFunsInvocationsInfo(problem: SyGuS16, name: String): Seq[Seq[String]] = {
    getSynthFunsInvocationsInfo(problem, Set(name))(name)
  }

  /**
    * Checks, if the constraints contains let terms, or composite terms as
    * arguments to the synth-function. Both of these cases make it impossible
    * to use GP test cases mode, because it requires concrete values as
    * arguments to the synth-function and not expressions.
    */
  def containsUnsupportedComplexTerms(problem: SyGuS16): Boolean = {
    try {
      checkUnsupportedTermsForGPMode(problem)
      false
    } catch { case _: Throwable => true }
  }

  def checkUnsupportedTermsForGPMode(problem: SyGuS16) {
    val synthFunNames = ExtractSynthesisTasks(problem).map(_.fname).toSet
    def checkExpr(term: Term, letVars: Set[String]): Unit = term match {
      case LetTerm(list, t) =>
        val newLetVars = list.map{case (name, _, _) => name}
        checkExpr(t, letVars ++ newLetVars)

      case CompositeTerm(symbol, args) if synthFunNames.contains(symbol) =>
        args.foreach {
          case LiteralTerm(_)   => true
          case SymbolTerm(name) =>
            if (letVars.contains(name))
              throw new UnsupportedFeatureException("Arguments to synthesized function cannot be bound by let expression.")
          case _ =>
            throw new UnsupportedFeatureException("Invocation of a synthesized function must take as an argument a literal or a variable.")
        }

      case c: CompositeTerm =>
        c.terms.foreach{ x: Term => checkExpr(x, letVars) }

      case _ => ()
    }
    problem.cmds.foreach{ case ConstraintCmd(term) => checkExpr(term, Set()) case _ => () }
  }
}



object LoadSygusBenchmark {
  def apply(path: String): SyGuS16 = {
    parseText(getBenchmarkContent(path))
  }
  def apply(file: File): SyGuS16 = {
    parseText(getBenchmarkContent(file))
  }

  def parseText(code: String, checkSupport: Boolean = true): SyGuS16 = {
    val parseRes = SyGuS16.parseSyGuS16Text(code)
    if (parseRes.isLeft)
      throw new Exception("PARSE ERROR: " + parseRes.left)
    assume(parseRes.isRight)
    val res = parseRes match { case Right(t) => t }
    res
  }

  def getBenchmarkContent(benchmark: String): String =
    getBenchmarkContent(new File(benchmark))

  def getBenchmarkContent(benchmark: File): String = {
    try {
      scala.io.Source.fromFile(benchmark).mkString
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

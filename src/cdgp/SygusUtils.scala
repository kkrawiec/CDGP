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
  * @param grammarSygus Grammar specifying the form of allowed programs.
  * @param args Arguments of the function.
  * @param outputType Output type of the function.
  */
case class SygusSynthesisTask(fname: String,
                              grammarSygus: Seq[(Any, Seq[Any])],
                              args: Seq[(String, SortExpr)],
                              outputType: SortExpr) {
  val argNames: Seq[String] = args.unzip._1
  val grammar: Grammar = SygusUtils.getSwimGrammar(grammarSygus)
  val canBeRecursive: Boolean = grammar.contains(Symbol(fname)) || grammar.contains(fname)

  /**
    * Returns code in SMTLIB of a synthesis function.
    */
  def getSynthFunCode(programBody: String): String = {
    val isRecursive = canBeRecursive && programBody.split("\\(|\\)|\\s+").contains(fname)
    val defFun = if (isRecursive) "define-fun-rec" else "define-fun"
    // (define-fun f (args) sort t) is equivalent to the following formulas:
    // (declare-fun f (args) sort)
    // (assert (forall (args) (= (f args) t))
    val sfArgs = SMTLIBFormatter.synthFunArgsToString(args)
    s"($defFun $fname ($sfArgs) ${SMTLIBFormatter.sortToString(outputType)} $programBody)"
  }
}


/**
  * Stores information about constraints of the given Sygus benchmark.
  * The two main types of constraints are:
  * - precond: constraints which are totally independent of the synthesis function.
  *     Dependency can for example mean that a certain function called in
  *     the constraints has in its body a call to synth-function.
  * - postcond: constraints dependent or containing the call to synth-function.
  *
  * Further, postconditions divide into two subclasses of constraints:
  * - testCasesConstr: constraints showing output of the synth-fun for a certain
  *     input. All synth-fun arguments and expected value are constants.
  * - formalConstr: Standard formal constraints, esp. containg calls to the synth-fun
  *     with universally quantified arguments.
  */
case class SygusBenchmarkConstraints(problem: SyGuS16, synthTask: SygusSynthesisTask,
                                     mixedSpecAllowed: Boolean = true) {
  val varDecls: Seq[VarDeclCmd] = problem.cmds.collect { case v: VarDeclCmd => v }
  val precond: Seq[ConstraintCmd] = SygusUtils.getPreconditions(problem)
  val postcond: Seq[ConstraintCmd] = SygusUtils.getPostconditions(problem)
  val (testCasesConstr, formalConstr) =
    if (mixedSpecAllowed) SygusUtils.divideOnTestsAndFormalConstr(postcond, synthTask)
    else (Seq(), postcond)
  lazy val allConstr: Seq[ConstraintCmd] = testCasesConstr ++ formalConstr
  lazy val formalInvocations: Seq[Seq[String]] = SygusUtils.getSynthFunsInvocationsInfo(formalConstr, synthTask.fname)
  lazy val allInvocations: Seq[Seq[String]] = SygusUtils.getSynthFunsInvocationsInfo(allConstr, synthTask.fname)

  /**
    * Creates test cases for the found testCasesConstr.
    */
  def testCasesConstrToTests: Seq[(Map[String, Any], Option[Any])] = {
    testCasesConstr.map { c =>
      val ct = c.t.asInstanceOf[CompositeTerm]
      val args = ct.terms(0).asInstanceOf[CompositeTerm].terms.map{ x=>
        SygusUtils.getValueOfLiteral(x.asInstanceOf[LiteralTerm].literal)
      }
      val input = synthTask.argNames.zip(args).toMap
      val output = SygusUtils.getValueOfLiteral(ct.terms(1).asInstanceOf[LiteralTerm].literal)
      (input, Some(output))
    }
  }
}





object SygusSynthesisTask {
  def apply(tree: SyGuS16): List[SygusSynthesisTask] = tree.cmds.collect {
    case SynthFunCmd14(sym: String, args: List[(String, SortExpr)], se: SortExpr, ntDefs: List[NTDef]) => {
      val grammar = retrieveGrammar(ntDefs)
      SygusSynthesisTask(sym, grammar, args, se) // name, function syntax, args list, output type
    }
    case SynthFunCmd16(sym: String, args: List[(String, SortExpr)], se: SortExpr) => {
      val grammarSygus = createDefaultGrammar(args, se)
      SygusSynthesisTask(sym, grammarSygus, args, se)
    }
  }

  /**
    * Creates a default grammar for Ints and Bools if it was not specified. Sort of the synth-fun is
    * used to determine the initial symbol. of the grammar.
    */
  def createDefaultGrammar(args: List[(String, SortExpr)], se: SortExpr): Seq[(Any, Seq[Any])] = {
    // Add the variables
    val bp = boolProd(args.filter(_._2 == BoolSortExpr()).map(_._1.toString))
    val ip = intProd(args.filter(_._2 == IntSortExpr()).map(_._1.toString))
    // The first symbol in the grammar is the initial symbol, and that symbol depends
    // on the output type of the function:
    se match {
      case BoolSortExpr() => List(bp, ip)
      case IntSortExpr()  => List(ip, bp)
      case _ => throw new Exception(s"Default grammar not supported for $se")
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







object SygusUtils {
  /**
    * Returns a set of fun-defined symbols which can be used only in postconditions.
    */
  def getPostcondSymbols(synthFunNames: Set[String], dmap: Map[String, Set[String]]): Set[String] = {
    def helper(post: Set[String], newDmap: Map[String, Set[String]]) = {
      val (newPost, newPre) = dmap.partition{ case (_, set) => set.exists(post.contains(_)) }
      if (newPost.isEmpty)
        post
      else
        getPostcondSymbols(post ++ newPost.keys, newPre)
    }
    helper(synthFunNames, dmap)
  }

  def getPostcondSymbols(problem: SyGuS16): Set[String] = {
    val synthFunNames = SygusSynthesisTask(problem).map(_.fname)
    val dmap = getFunDefDependencyMap(problem)
    getPostcondSymbols(synthFunNames.toSet, dmap)
  }

  /**
    * Returns all constraints which are preconditions of the synthesis task.
    * Preconditions are constraints which may contain only:
    * (a) variables introduced with declare-var
    * (b) constants
    * (c) macros, which themselves contain only elements specified in (a), (b) or (c).
    */
  def getPreconditions(problem: SyGuS16): Seq[ConstraintCmd] = {
    val postSymbols = getPostcondSymbols(problem)
    problem.cmds.filter{case ConstraintCmd(term) => isPrecondition(term, postSymbols); case _ => false}.
      asInstanceOf[Seq[ConstraintCmd]]
  }

  /**
    * Returns all constraints which are preconditions of the synthesis task.
    * Postconditions depends, directly or indirectly, on the result of the function being
    * synthesized.
    */
  def getPostconditions(problem: SyGuS16): Seq[ConstraintCmd] = {
    val postSymbols = getPostcondSymbols(problem)
    problem.cmds.filter{case ConstraintCmd(term) =>
      !isPrecondition(term, postSymbols); case _ => false}.asInstanceOf[Seq[ConstraintCmd]]
  }

  private def isPrecondition(term: Term, postSymbols: Set[String]): Boolean = {
    val freeSymbs = collectFreeSymbols(term)
    !freeSymbs.exists{ v: String => postSymbols.contains(v) }
  }


  /**
    * For a given list of constraints finds all constraint that are a simple equality
    * constraints, that is they are of the form CompositeTerm(=, sf(args), c),
    * where sf is synth-function, args are constant arguments and c is a
    * constant output.
    *
    * In the CompositeTerm arguments may be reversed. This function will return
    * standardized ConstraintCmd with synthFun always on the left side.
    */
  def divideOnTestsAndFormalConstr(constrCmds: Seq[ConstraintCmd], synthFun: SygusSynthesisTask): (Seq[ConstraintCmd], Seq[ConstraintCmd]) = {
    def checkEqualityArgs(eqTerm: CompositeTerm): (Boolean, Option[CompositeTerm]) = {
      assert(eqTerm.symbol == "=")
      def args = eqTerm.terms
      val (sfTerm, outTerm): (CompositeTerm, LiteralTerm) =
        if (args(0).isInstanceOf[CompositeTerm] && args(1).isInstanceOf[LiteralTerm])
          (args(0).asInstanceOf[CompositeTerm], args(1).asInstanceOf[LiteralTerm])
        else if (args(1).isInstanceOf[CompositeTerm] && args(0).isInstanceOf[LiteralTerm])
          (args(1).asInstanceOf[CompositeTerm], args(0).asInstanceOf[LiteralTerm])
        else
          return (false, None)

      if (!(sfTerm.symbol == synthFun.fname && sfTerm.terms.forall(_.isInstanceOf[LiteralTerm])))
        (false, None)
      else
        (true, Some(CompositeTerm("=", List(sfTerm, outTerm))))
    }

    val processedConstr = constrCmds.map { cmd => cmd.t match {
        case c @ CompositeTerm("=", args) if args.size == 2 =>
          val res = checkEqualityArgs(c)
          if (res._1) (true, ConstraintCmd(res._2.get))
          else (false, cmd)
        case _ => (false, cmd)
      }
    }
    val (tConstr, fConstr) = processedConstr.partition(_._1)
    (tConstr.map(_._2), fConstr.map(_._2))
  }


  def getValueOfLiteral(literal: Literal): Any = {
    literal match {
      case IntConst(v) => v
      case RealConst(v) => v
      case BoolConst(v) => v
      case BVConst(v) => v
      case StringConst(v) => v
      case _ => throw new Exception("Unsupported literal type!")
    }
  }

  /**
    * Solver requires renaming of the variables, so that it is able to handle cases
    * similar to the one below. This renaming is only needed for the execution of the
    * program by the domain.
    *   (synth-fun synthFun ((x Int)(y Int)) ... )
    *   (declare-var a Int)
    *   (declare-var b Int)
    *   (constraint (=> (= (- b a) (- c b)) (= (synthFun a b) 1)))
    *
    * For example:
    *   testInputsMap: Map(a -> -54, b -> 2)
    *   synFunArgNames: List(x, y)
    *   invNames: List(a, 300)
    * will lead to the result:
    *   Map(x -> -54, y -> 300)
    */
  def renameVars(testInputsMap: Map[String, Any], invNames: Seq[String],
                 synFunArgNames: Seq[String]): Map[String, Any] = {
    invNames.zip(synFunArgNames).map{
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
  def renameVarsInTerm(p: Term, map: Map[String, String]): Term = {
    p match {
      case CompositeTerm(name, terms) => CompositeTerm(name, terms.map(renameVarsInTerm(_, map)))
      case SymbolTerm(symb) => SymbolTerm(map.getOrElse(symb, symb))
      case LetTerm(list, term) => LetTerm(list, renameVarsInTerm(term, map))
      case ExistsTerm(list, term) => ExistsTerm(list, renameVarsInTerm(term, map))
      case ForallTerm(list, term) => ForallTerm(list, renameVarsInTerm(term, map))
      case x => x
    }
  }

  def getDeclaredFunNames(problem: SyGuS16): Set[String] = {
    problem.cmds.collect{ case FunDeclCmd(name, _, _) => name}.toSet
  }

  def getDeclaredVarNames(problem: SyGuS16): Set[String] = {
    problem.cmds.collect{ case VarDeclCmd(name, _) => name}.toSet
  }

  /**
    * Returns a map assigning to each symbol symbols it is dependent on. This is later
    * used to establish, if a certain constraint belongs to preconditions or postconditions.
    */
  def getFunDefDependencyMap(problem: SyGuS16): Map[String, Set[String]] = {
    problem.cmds.collect{
      case FunDefCmd(name, args, _, term) => (name, collectFreeSymbols(term, args.map(_._1).toSet))
    }.toMap
  }

  /**
    * Collects names of all free variables in the term.
    */
  def collectFreeVars(p: Term, boundVars: Set[String] = Set()): Set[String] = {
    p match {
      case CompositeTerm(name, terms) => terms.flatMap(collectFreeVars(_, boundVars)).toSet -- boundVars
      case SymbolTerm(symb) => Set(symb) -- boundVars
      case LetTerm(list, term) => collectFreeVars(term, boundVars ++ list.map(_._1))
      case ExistsTerm(list, term) => collectFreeVars(term, boundVars ++ list.map(_._1))
      case ForallTerm(list, term) => collectFreeVars(term, boundVars ++ list.map(_._1))
      case _ => Set()
    }
  }

  /**
    * Collects names of all free variables in the constraint commands.
    */
  def collectFreeVars(cmds: Seq[Cmd]): Set[String] = {
    cmds.collect {
      case ConstraintCmd(t) => collectFreeVars(t)
    }.foldRight(Set[String]()){ (a, b) => a ++ b }
  }

  /**
    * Collects names of all free symbols (i.e. symbols not bounded by let expression)
    * in the term. Function names are included.
    */
  def collectFreeSymbols(p: Term, boundVars: Set[String] = Set()): Set[String] = {
    p match {
      case CompositeTerm(name, terms) => (terms.flatMap(collectFreeSymbols(_, boundVars)).toSet + name) -- boundVars
      case SymbolTerm(symb) => Set(symb) -- boundVars
      case LetTerm(list, term) => collectFreeSymbols(term, boundVars ++ list.map(_._1))
      case ExistsTerm(list, term) => collectFreeSymbols(term, boundVars ++ list.map(_._1))
      case ForallTerm(list, term) => collectFreeSymbols(term, boundVars ++ list.map(_._1))
      case _ => Set()
    }
  }

  /**
    * Collects names of all functions used in the term.
    */
  def collectFunctionNames(p: Term): Set[String] = {
    p match {
      case CompositeTerm(name, terms) => terms.flatMap(collectFunctionNames(_)).toSet + name
      case SymbolTerm(symb) => Set()
      case LetTerm(list, term) => collectFunctionNames(term)
      case ExistsTerm(list, term) => collectFunctionNames(term)
      case ForallTerm(list, term) => collectFunctionNames(term)
      case _ => Set()
    }
  }

  /**
    * Changes names of *all* terms in the expression, including names of functions.
    * The map contains mapping between old and new names and does not have to be complete.
    */
  def renameNamesInTerm(p: Term, map: Map[String, String]): Term = {
    p match {
      case CompositeTerm(name, terms) =>
        CompositeTerm(map.getOrElse(name, name), terms.map(renameNamesInTerm(_, map)))
      case SymbolTerm(symb) => SymbolTerm(map.getOrElse(symb, symb))
      case LetTerm(list, term) => LetTerm(list, renameNamesInTerm(term, map))
      case ExistsTerm(list, term) => ExistsTerm(list, renameNamesInTerm(term, map))
      case ForallTerm(list, term) => ForallTerm(list, renameNamesInTerm(term, map))
      case x => x
    }
  }

  /**
    * Checks, if a given problem has the single invocation property. This property
    * states that if there is a function f, then every invocation of this function
    * in the constraints takes exactly the same arguments.
    *
    * Consider function f(x,y). The following constraint has the property:
    * f(x,y) >= x && f(x,y) >= y
    * while this one does not:
    * f(x,y) == f(y,x)
    */
  def hasSingleInvocationPropertyAllConstr(problem: SyGuS16): Boolean = {
    val sfs = SygusSynthesisTask(problem)
    val setNames = sfs.map(_.fname).toSet
    val constrCmds = getAllConstraints(problem)
    val invInfo = getSynthFunsInvocationsInfo(constrCmds, setNames)
    invInfo.forall{ case (n, lst) => lst.toSet.size == 1}
  }


  /**
    * Checks, if a synthesis task has the single invocation property in the given
    * set of constraints. This property states that if there is a function f,
    * then every invocation of this function in the constraints takes exactly
    * the same arguments.
    *
    * Consider function f(x,y). The following constraint has the property:
    * f(x,y) >= x && f(x,y) >= y
    * while this one does not:
    * f(x,y) == f(y,x)
    */
  def hasSingleInvocationProperty(synthTask: SygusSynthesisTask,
                                  constrCmds: Seq[ConstraintCmd]): Boolean = {
    val invInfo = getSynthFunsInvocationsInfo(constrCmds, Set(synthTask.fname))
    invInfo.forall{ case (n, lst) => lst.toSet.size == 1}
  }
  def hasSingleInvocationProperty(data: SygusBenchmarkConstraints): Boolean =
    hasSingleInvocationProperty(data.synthTask, data.formalConstr)

  /**
    * Creates a map assigning to each provided synth function a list of arguments
    * this function was invoked with in the constraints. Each invocation is represented
    * as a distinct entry, so there may be duplicates.
    */
  def getSynthFunsInvocationsInfo(constrCmds: Seq[ConstraintCmd], setNames: Set[String]): Map[String, Seq[Seq[String]]] = {
    def searchExpr(p: Term): List[(String, List[String])] = p match {
      case c: CompositeTerm if setNames.contains(c.symbol) =>
        val tup = (c.symbol, c.terms.map{
          case LiteralTerm(v) => v.toString
          case SymbolTerm(s) => s
          case x @ CompositeTerm(_, _) => x.toString
        })
        List(tup)
      case CompositeTerm(_, terms) => terms.flatMap{ x: Term => searchExpr(x) }
      case LetTerm(_, term) => searchExpr(term)
      case ExistsTerm(_, term) => searchExpr(term)
      case ForallTerm(_, term) => searchExpr(term)
      case _ => List()
    }
    val collected: Seq[(String, List[String])] = constrCmds.flatMap { cmd => searchExpr(cmd.t) }
    collected.groupBy(_._1).map{ case (k, v) => (k, v.map(_._2)) }
  }

  def getSynthFunsInvocationsInfo(constrCmds: Seq[ConstraintCmd], name: String): Seq[Seq[String]] = {
    getSynthFunsInvocationsInfo(constrCmds, Set(name))(name)
  }
  def getSynthFunsInvocationsInfo(problem: SyGuS16, name: String): Seq[Seq[String]] = {
    getSynthFunsInvocationsInfo(getAllConstraints(problem), Set(name))(name)
  }

  def getAllConstraints(problem: SyGuS16): Seq[ConstraintCmd] = {
    problem.cmds.collect {case c @ ConstraintCmd(_) => c}
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
    val synthFunNames = SygusSynthesisTask(problem).map(_.fname).toSet
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

  /**
    * Converts SySuS grammar into SWIM grammar.
    */
  def getSwimGrammar(grammarSygus: Seq[(Any, Seq[Any])]): Grammar = {
    val grammarMap = grammarSygus.toMap
    val start = if (!grammarMap.contains("Start")) grammarSygus.head._1 else "Start"
    Grammar.fromMap(start, grammarMap)
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

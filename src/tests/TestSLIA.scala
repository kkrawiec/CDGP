package tests

import java.io.File

import org.junit.Test
import org.junit.Assert._
import cdgp._
import fuel.func.RunExperiment
import fuel.util.IApp
import swim.tree.{CodeFactory, Op, SimpleGP}
import sygus.{BoolSortExpr, IntSortExpr, VarDeclCmd}



final class TestLIA {
  @Test
  def test_LIA(): Unit = {
    val domainLIA = SLIA(List("x", "y", "z"), "rec")
    val inputs = Seq(2, 10, 6)
    val op = Op('nt, "x")
    assertEquals(2, domainLIA(op)(inputs).get)
    val op2 = Op('nt, "+", Op('nt, 3), Op('nt, "z"))
    assertEquals(9, domainLIA(op2)(inputs).get)
    val op3 = Op.fromStr("ite(<=(x 0) 0 +(2 rec(-(x 1) y z)))", useSymbols = false)
    assertEquals(4, domainLIA(op3)(inputs).get)

    val semantics = domainLIA.operationalSemantics(Seq()) _

    // More information about String in CVC4: http://cvc4.cs.stanford.edu/wiki/Strings
    // Script to verify below expected answers: resources/str_test.smt2

    // str.prefixof
    assertEquals(true, semantics(Seq("str.prefixof", "", "")))
    assertEquals(true, semantics(Seq("str.prefixof", "", "asd")))
    assertEquals(false, semantics(Seq("str.prefixof", "asd", "")))
    assertEquals(true, semantics(Seq("str.prefixof", "asd", "asd")))
    assertEquals(true, semantics(Seq("str.prefixof", "asd", "asda")))
    assertEquals(false, semantics(Seq("str.prefixof", "da", "asda")))

    // str.suffixof
    assertEquals(true, semantics(Seq("str.suffixof", "", "")))
    assertEquals(true, semantics(Seq("str.suffixof", "", "asd")))
    assertEquals(false, semantics(Seq("str.suffixof", "asd", "")))
    assertEquals(true, semantics(Seq("str.suffixof", "asd", "asd")))
    assertEquals(false, semantics(Seq("str.suffixof", "asd", "asda")))
    assertEquals(true, semantics(Seq("str.suffixof", "da", "asda")))

    // str.at
    assertEquals("", semantics(Seq("str.at", "", -1)))
    assertEquals("", semantics(Seq("str.at", "", 0)))
    assertEquals("", semantics(Seq("str.at", "", 1)))
    assertEquals("", semantics(Seq("str.at", "a", -1)))
    assertEquals("a", semantics(Seq("str.at", "a", 0)))
    assertEquals("", semantics(Seq("str.at", "a", 1)))
    assertEquals("b", semantics(Seq("str.at", "ab", 1)))

    // str.contains
    assertEquals(true, semantics(Seq("str.contains", "", "")))
    assertEquals(true, semantics(Seq("str.contains", "asda", "")))
    assertEquals(false, semantics(Seq("str.contains", "asda", "ad")))
    assertEquals(true, semantics(Seq("str.contains", "aaa", "aa")))
    assertEquals(false, semantics(Seq("str.contains", "asda", "z")))

    // str.replace: in SMTLIB only the *first* occurrence is replaced
    assertEquals("", semantics(Seq("str.replace", "", "", "")))
    assertEquals("", semantics(Seq("str.replace", "", "", "5")))
    assertEquals("x", semantics(Seq("str.replace", "x", "", "5")))
    assertEquals("xx", semantics(Seq("str.replace", "xx", "", "5")))
    assertEquals("5a", semantics(Seq("str.replace", "aaa", "aa", "5")))
    assertEquals("5sda", semantics(Seq("str.replace", "asda", "a", "5")))

    // str.indexof
    assertEquals( 0, semantics(Seq("str.indexof", "", "", 0)))
    assertEquals(-1, semantics(Seq("str.indexof", "", "", -1)))
    assertEquals(-1, semantics(Seq("str.indexof", "", "", 1)))
    assertEquals(-1, semantics(Seq("str.indexof", "a", "", -1)))
    assertEquals( 0, semantics(Seq("str.indexof", "a", "", 0)))
    assertEquals( 1, semantics(Seq("str.indexof", "a", "", 1)))
    assertEquals(-1, semantics(Seq("str.indexof", "a", "", 2)))
    assertEquals(-1, semantics(Seq("str.indexof", "aa", "", -1)))
    assertEquals( 0, semantics(Seq("str.indexof", "aa", "", 0)))
    assertEquals( 1, semantics(Seq("str.indexof", "aa", "", 1)))
    assertEquals( 2, semantics(Seq("str.indexof", "aa", "", 2)))
    assertEquals(-1, semantics(Seq("str.indexof", "aa", "", 3)))
    assertEquals(-1, semantics(Seq("str.indexof", "a", "b", 0)))
    assertEquals( 0, semantics(Seq("str.indexof", "a", "a", 0)))
    assertEquals(-1, semantics(Seq("str.indexof", "aaa", "a", -1)))
    assertEquals( 0, semantics(Seq("str.indexof", "aaa", "a", 0)))
    assertEquals( 1, semantics(Seq("str.indexof", "aaa", "a", 1)))
    assertEquals( 2, semantics(Seq("str.indexof", "aaa", "a", 2)))
    assertEquals(-1, semantics(Seq("str.indexof", "aaa", "a", 3)))
    assertEquals( 2, semantics(Seq("str.indexof", "ssaaa", "aa", 0)))
    assertEquals( 0, semantics(Seq("str.indexof", "aaa", "aa", 0)))
    assertEquals( 1, semantics(Seq("str.indexof", "aaa", "aa", 1)))
    assertEquals(-1, semantics(Seq("str.indexof", "aaa", "aa", 2)))
    assertEquals(-1, semantics(Seq("str.indexof", "asda", "aa", 0)))

    // str.substr
    assertEquals("", semantics(Seq("str.substr", "", -1, 2)))
    assertEquals("", semantics(Seq("str.substr", "asd", -1, 2)))
    assertEquals("", semantics(Seq("str.substr", "", 0, 0)))
    assertEquals("", semantics(Seq("str.substr", "", 0, 1)))
    assertEquals("", semantics(Seq("str.substr", "as", -1, -1)))
    assertEquals("as", semantics(Seq("str.substr", "asdfgh", 0, 2)))
    assertEquals("asdfgh", semantics(Seq("str.substr", "asdfgh", 0, 10)))
    assertEquals("fgh", semantics(Seq("str.substr", "asdfgh", 3, 10)))
    assertEquals("", semantics(Seq("str.substr", "asdfgh", 10, 10)))
    assertEquals("asdfgh", semantics(Seq("str.substr", "asdfgh", 0, 100)))
    assertEquals("", semantics(Seq("str.substr", "asdfgh", 0, -1)))
    assertEquals("", semantics(Seq("str.substr", "asdfgh", 3, 0)))
    assertEquals("f", semantics(Seq("str.substr", "asdfgh", 3, 1)))
    assertEquals("fg", semantics(Seq("str.substr", "asdfgh", 3, 2)))

    // str.to.int
    assertEquals(-1, semantics(Seq("str.to.int", "")))
    assertEquals(-1, semantics(Seq("str.to.int", "-123")))
    assertEquals(-1, semantics(Seq("str.to.int", "(- 123)")))
    assertEquals(1, semantics(Seq("str.to.int", "1")))
    assertEquals(1234567, semantics(Seq("str.to.int", "1234567")))

    // int.to.str
    assertEquals("0", semantics(Seq("int.to.str", 0)))
    assertEquals("", semantics(Seq("int.to.str", -123)))
    assertEquals("", semantics(Seq("int.to.str", -12)))
    assertEquals("1", semantics(Seq("int.to.str", 1)))
    assertEquals("1234567", semantics(Seq("int.to.str", 1234567)))
  }
}


object TestRunLIA extends IApp('maxGenerations -> 25, 'printResults -> false, 'populationSize -> 25,
  'initMaxTreeDepth -> 7, 'maxSubtreeDepth -> 5, 'parEval -> false) {

  val root = System.getProperty("user.dir")
  println(s"Working directory: $root")

  val collection = "/resources/LIA/other"
  val files = Tools.getRecursiveListOfFiles(new File(root + collection)).filter{ f =>
      !f.getName.matches(".+[0-9][0-9].+") && f.getName.endsWith(".sl")}
  val solverPath = Global.solverPath

  for (file <- files) {
    println("-" * 100)
    println(s"File: ${file.getAbsolutePath}")
    println("-" * 100)
    val sygusProblem = LoadSygusBenchmark(file)
    val sygusData = SygusProblemData(sygusProblem)
    def synthTask = sygusData.synthTask

    // Create the Swim grammar from it
    val gr = synthTask.grammar

    // Generate a bunch of random programs using the grammar
    val cf = new CodeFactory(gr, stoppingDepth = 4, maxDepth = 8)
    val progs = for (i <- 0 until 10) yield cf.randomProgram

    ////////////////////////////////////////////////////////////////
    // Apply the programs to a random input
    val input = synthTask.args.collect {
      case (name, IntSortExpr())  => name -> (rng.nextInt(21) - 10)
      case (name, BoolSortExpr()) => name -> rng.nextBoolean
    }
    val inputAsMap: Map[String, Any] = input.toMap
    val domainLIA = SLIA(synthTask.argNames, synthTask.fname)
    for (p <- progs)
      try {
        println("Program " + p + " applied to " + input.unzip._2 +
          " evaluates to " + domainLIA(p)(inputAsMap.toSeq.map(_._2)))
      }
      catch { case e: Throwable => println(s"Error during evalution: ${e.getMessage}") }

    ////////////////////////////////////////////////////////////////
    // Run a naive GP with randomly generated input as the only test
    println("GP run:")
    val inValues: Seq[Any] = synthTask.argNames.map(inputAsMap(_))
    val tests = Seq(swim.Test[Seq[Any], Option[Any]](inValues, Some(0)))
    try {
      println("Tests: " + tests)
      val alg = SimpleGP.Discrete(gr, domainLIA, tests)
      RunExperiment(alg)
      println("Best solution: " + alg.bsf.bestSoFar)
    }
    catch { case e: Throwable => println(s"Error during evalution: ${e.getMessage}") }

    ////////////////////////////////////////////////////////////////
    // Verifying programs using SMT solver
    println("Verifying programs:")
    val solver = SolverInteractive(solverPath, verbose = false)
    val fv = sygusProblem.cmds.collect { case v: VarDeclCmd => v }

    val templateInputAndKnownOutput = new TemplateIsOutputCorrectForInput(sygusProblem, sygusData)
    val templateVerify = new TemplateVerification(sygusProblem, sygusData)
    for (p <- progs) {
      // Prepare input to the solver
      val verificationProblem = templateVerify(p)
      // Run the solver:
      val (_, res) = solver.solve(verificationProblem)
      if (res.isDefined) {
        val cexample = GetValueParser(res.get)
        // IMPORTANT: To run a program on the counterexample, need to rename the values of variables
        // IMPORTANT: This assumes that the free variables defined in the problem correspond one-to-one 
        // (order-preserving) to the arguments of synthesized function.
        val cexampleRenamed = synthTask.argNames.zip(cexample.unzip._2)
        println(s"Counterexample for $p: " + cexampleRenamed)
        try {
          val output = domainLIA.apply(p)(cexampleRenamed.map(_._2)).get
          println(s"Output of best: $output")
          val checkOnInputCmd = templateInputAndKnownOutput(cexample.toMap, output)
          println("Check output for counterexample (expected unsat): " + solver.solve(checkOnInputCmd))
        }
        catch {
          case e: Throwable => println(s"Error during evalution: ${e.getMessage}")
        }
      }
      else { println("Correct program found") }
    }
  }
}

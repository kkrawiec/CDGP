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

    assertEquals(true, domainLIA.operationalSemantics(Seq())(Seq("str.contains", "asd", "")))
    assertEquals(true, domainLIA.operationalSemantics(Seq())(Seq("str.contains", "", "")))
    // In String logic empty string can be a prefix or suffix only of the nonempty string
    assertEquals(true, domainLIA.operationalSemantics(Seq())(Seq("str.suffixof", "", "")))
    assertEquals(false, domainLIA.operationalSemantics(Seq())(Seq("str.suffixof", "x", "")))
    assertEquals(true, domainLIA.operationalSemantics(Seq())(Seq("str.prefixof", "", "")))
    assertEquals(false, domainLIA.operationalSemantics(Seq())(Seq("str.prefixof", "x", "")))
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

    // Retrieve the grammar and signature of the function to be synthesized
    val synthTask = ExtractSynthesisTasks(sygusProblem).head

    // Create the Swim grammar from it
    val gr = synthTask.grammar

    // Generate a bunch of random programs using the grammar
    val cf = new CodeFactory(gr, stoppingDepth = 4, maxDepth = 8)
    val progs = for (i <- 0 until 10) yield cf.randomProgram

    ////////////////////////////////////////////////////////////////
    // Apply the programs to a random input
    val input = synthTask.args.map {
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
    val getValueCommand = f"(get-value (${fv.map(_.sym).mkString(" ")}))"

    for (p <- progs) {
      // Prepare input to the solver
      val verificationProblem = SMTLIBFormatter.verifyProblem(synthTask, sygusProblem, p)
      // Run the solver:
      val (_, res) = solver.solve(verificationProblem, getValueCommand)
      if (res.isDefined) {
        val cexample = GetValueParser(res.get)
        // IMPORTANT: To run a program on the counterexample, need to rename the values of variables
        // IMPORTANT: This assumes that the free variables defined in the problem correspond one-to-one 
        // (order-preserving) to the arguments of synthesized function.
        val cexampleRenamed = synthTask.argNames.zip(cexample.unzip._2)
        println("Counterexample: " + cexampleRenamed)

        try {
          val output = domainLIA.apply(p)(cexampleRenamed.map(_._2)).get
          println(s"Output of best: $output")
          val checkOnTestCmd = SMTLIBFormatter.checkOnInputAndKnownOutput(synthTask, sygusProblem, cexample.toMap, output)
          println("Check output for counterexample (expected unsat): " + solver.solve(checkOnTestCmd))
        }
        catch {
          case e: Throwable => println(s"Error during evalution: ${e.getMessage}")
        }
      }
      else { println("Correct program found") }
    }
  }
}

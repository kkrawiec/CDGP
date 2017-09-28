package tests

import java.io.File

import cdgp._
import fuel.func.RunExperiment
import fuel.util.IApp
import swim.Test
import swim.tree.{CodeFactory, SimpleGP}
import sygus.{BoolSortExpr, IntSortExpr, VarDeclCmd}

import scala.collection.immutable.{Map, Seq}


object TestLIA extends IApp('maxGenerations -> 25, 'printResults -> false, 'populationSize -> 25,
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
    val gr = ExtractSygusGrammar(synthTask)

    // Generate a bunch of random programs using the grammar
    val cf = new CodeFactory(gr, stoppingDepth = 4, maxDepth = 8)
    val progs = for (i <- 0 until 10) yield cf.randomProgram

    ////////////////////////////////////////////////////////////////
    // Apply the programs to a random input
    val input = synthTask.arguments.map {
      case (name, IntSortExpr())  => name -> (rng.nextInt(21) - 10)
      case (name, BoolSortExpr()) => name -> rng.nextBoolean
    }
    val inputAsMap = input.toMap
    for (p <- progs)
      try {
        println("Program " + p + " applied to " + input.unzip._2 +
          " evaluates to " + LIA(p)(inputAsMap))
      }
      catch { case e: Throwable => println(s"Error during evalution: ${e.getMessage}") }

    ////////////////////////////////////////////////////////////////
    // Run a naive GP with randomly generated input as the only test
    println("GP run:")
    val tests = Seq(Test[Map[String, Any], Any](inputAsMap, 0))
    try {
      println("Tests: " + tests)
      val alg = SimpleGP.Discrete(gr, LIA, tests)
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
      val verificationProblem = SMTLIBFormatter.verifyProblem(sygusProblem, p)
      // Run the solver:
      val (_, res) = solver.solve(verificationProblem, getValueCommand)
      if (res.isDefined) {
        val cexample = GetValueParser(res.get)
        // IMPORTANT: To run a program on the counterexample, need to rename the values of variables
        // IMPORTANT: This assumes that the free variables defined in the problem correspond one-to-one 
        // (order-preserving) to the arguments of synthesized function.
        val cexampleRenamed = input.unzip._1.zip(cexample.unzip._2)
        println("Counterexample: " + cexampleRenamed)

        try {
          val output = LIA.apply(p)(cexampleRenamed.toMap)
          println(s"Output of best: $output")
          val checkOnTestCmd = SMTLIBFormatter.checkOnInputAndKnownOutput(sygusProblem, cexample.toMap, output)
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

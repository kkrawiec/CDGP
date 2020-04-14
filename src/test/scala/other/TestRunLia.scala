import java.io.File

import cdgp._
import fuel.func.RunExperiment
import fuel.util.IApp
import swim.tree.{CodeFactory, SimpleGP}
import sygus.{BoolSortExpr, IntSortExpr, VarDeclCmd}
import tests.Global


object TestRunLIA extends IApp('maxGenerations -> 25, 'printResults -> false, 'populationSize -> 25,
  'initMaxTreeDepth -> 7, 'maxSubtreeDepth -> 5, 'parEval -> false) {

  val root = System.getProperty("user.dir")
  println(s"Working directory: $root")

  val collection = "/resources/LIA/cdgp_paper17"
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
    val gr = synthTask.uninterpSwimGrammar

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
    val domainLIA = DomainSLIA(synthTask.argNames, Symbol(synthTask.fname))
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

    val templateInputAndKnownOutput = new TemplateIsOutputCorrectForInput(sygusData)
    val templateVerify = new TemplateVerification(sygusData)
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
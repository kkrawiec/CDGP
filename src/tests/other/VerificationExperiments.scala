package tests.other

import java.io.File

import cdgp._
import fuel.func.RunExperiment
import fuel.util.IApp
import swim.tree.{CodeFactory, SimpleGP}
import sygus.{BoolSortExpr, IntSortExpr, StringSortExpr, VarDeclCmd}
import tests.Global
import tests.TestRunLIA.rng


object VerificationExperiments extends App {
  val root = System.getProperty("user.dir")
  println(s"Working directory: $root")

  def randomString(length: Int) = {
    val r = new scala.util.Random
    val sb = new StringBuilder
    for (i <- 1 to length) {
      sb.append(r.nextPrintableChar)
    }
    sb.toString
  }

  1.to(20).foreach{ x => println(randomString(6)) }

  val collection = "/resources/LIA/other"
  //val files = Tools.getRecursiveListOfFiles(new File(root + collection)).filter{ f =>
  //  !f.getName.matches(".+[0-9][0-9].+") && f.getName.endsWith(".sl")}
  val files = List(new File(root + "/resources/LIA/Median3_tests.sl"))
  val solverPath = Global.solverPath

  for (file <- files) {
    println("-" * 100)
    println(s"File: ${file.getAbsolutePath}")
    println("-" * 100)
    val sygusProblem = LoadSygusBenchmark(file)
    val synthTask = SygusSynthesisTask(sygusProblem).head
    val sygusConstr = SygusBenchmarkConstraints(sygusProblem, synthTask)

    // Create the Swim grammar and generate a bunch of random programs using it
    val gr = synthTask.grammar
    val cf = new CodeFactory(gr, stoppingDepth = 4, maxDepth = 8)
    val progs = for (i <- 0 until 100) yield cf.randomProgram
    val domainLIA = SLIA(synthTask.argNames, synthTask.fname)


    // Verifying programs using SMT solver
    println("Verifying programs:")
    val solver = SolverInteractive(solverPath, verbose = false)
    val fv = sygusProblem.cmds.collect { case v: VarDeclCmd => v }
    val getValueCommand = s"(get-value (${fv.map(_.sym).mkString(" ")}))"

    val templateInputAndKnownOutput = new QueryTemplateInputAndKnownOutput(sygusProblem, sygusConstr)
    val templateVerify = new QueryTemplateVerification(sygusProblem, sygusConstr)
    for (p <- progs) {
      println("Program: " + p)
      // Prepare input to the solver
      val verificationProblem = templateVerify(p)
      val (_, res) = solver.solve(verificationProblem, getValueCommand)
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


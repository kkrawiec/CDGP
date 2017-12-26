package tests.other

import java.io.File

import cdgp._
import fuel.util.{Options, Rng}
import swim.tree.{CodeFactory, Op}
import tests.Global

import scala.collection.mutable


object VerificationExperiments extends App {
  implicit val rng = Rng(Options("--seed 0"))

  def randomString(length: Int) = {
    val r = new scala.util.Random
    val sb = new StringBuilder
    for (i <- 1 to length) {
      sb.append(r.nextPrintableChar)
    }
    sb.toString
  }

  def generateRandomPrograms(synthTask: SygusSynthTask): Seq[Op] = {
    val cf = new CodeFactory(synthTask.getSwimGrammar(rng), stoppingDepth = 4, maxDepth = 8)
    for (i <- 0 until 100) yield cf.randomProgram
  }


  val root = System.getProperty("user.dir")
  println(s"Working directory: $root")

  1.to(20).foreach{ x => println(randomString(6)) }

  val collection = "/resources/LIA/other"
  //val files = Tools.getRecursiveListOfFiles(new File(root + collection)).filter{ f =>
  //  !f.getName.matches(".+[0-9][0-9].+") && f.getName.endsWith(".sl")}
  val files = List(new File(root + "/resources/LIA/Median3_t.sl"))
  val solver = SolverInteractive(Global.solverPath, verbose = false)

  for (file <- files) {
    println("-" * 100)
    println(s"File: ${file.getAbsolutePath}")
    println("-" * 100)
    val sygusProblem = LoadSygusBenchmark(file)
    val sygusData = SygusProblemData(sygusProblem)
    def synthTask = sygusData.synthTask
    val tests = sygusData.testCasesConstrToTests
    val testsSeq = tests.map{ test => test._1.toList.map(_._2)}
    val domainLIA = DomainSLIA(synthTask.argNames, Symbol(synthTask.fname))


    val fv = sygusData.varDecls
    val templateFindOutput = new TemplateFindOutput(sygusProblem, sygusData)
    val templateVerify = new TemplateVerification(sygusProblem, sygusData)

    val progs = generateRandomPrograms(synthTask)
    val allCounterEx = mutable.MutableList[Map[String,Any]]()
    for (p <- progs) {
      println("Program: " + p)
      val verificationProblem = templateVerify(p)
      // println("verificationProblem:\n" + verificationProblem)
      val (_, res) = solver.solve(verificationProblem)
      if (res.isDefined) {
        val cexample = GetValueParser(res.get)
        // IMPORTANT: To run a program on the counterexample, need to rename the values of variables
        // IMPORTANT: This assumes that the free variables defined in the problem correspond one-to-one
        // (order-preserving) to the arguments of synthesized function.
        val cexampleRenamed = synthTask.argNames.zip(cexample.unzip._2)
        allCounterEx += cexampleRenamed.toMap
        println("\tCounterexample: " + cexampleRenamed.mkString(", "))
        try {
          val output = domainLIA.apply(p)(cexampleRenamed.map(_._2)).get
          println(s"\tProgram's output (LIA domain): $output")
          testsSeq.foreach{ test =>
            println(s"\tProgram's output for $test: " + domainLIA.apply(p)(test).get) }
          val findOutputProblem = templateFindOutput(cexample.toMap)
          // println("findOutputProblem:\n" + findOutputProblem)
          val (_, res) = solver.solve(findOutputProblem)
          println("\tExpected output for counterex: " + GetValueParser(res.get).head._2)
        }
        catch {
          case e: Throwable => println(s"	Error during evalution: ${e.getMessage}")
        }
      }
      else { println("\tCorrect program found") }
    }

    println(s"\nCollected ${allCounterEx.toSet.size} unique counterexamples (total: ${allCounterEx.size})")
    allCounterEx.toSet.foreach{ x: Map[String, Any] => println(x) }
  }
}


package sygusgp

import java.io.File

import scala.collection.immutable.Map
import scala.collection.immutable.Seq

import fuel.func.RunExperiment
import fuel.util.IApp
import swim.Grammar
import swim.Test
import swim.tree.CodeFactory
import swim.tree.SimpleGP
import sygus14.SyGuS14
import sygus.BoolSortExpr
import sygus.VarDeclCmd
import sygus16.SyGuS16
import sygus.IntSortExpr

object TestLIA extends IApp('maxGenerations -> 300, 'printResults -> true, 'populationSize -> 100,
  'initMaxTreeDepth -> 7, 'maxSubtreeDepth -> 5 //'parEval -> false
  ) {

  val root = System.getProperty("user.dir")
  // 0 for one small test, anything else for running on many Sygus benchmarks
  //val collection ="/../SyGuS/resources/sygus14/integer-benchmarks/"
  val collection = "/../ProgramSynthesisBenchmarks/resources/sygus16/"
  val files = 1 match {
    case 0 => List(new File(root + "/../SyGuS/resources/example.txt"))
    case _ => Tools.getRecursiveListOfFiles(new File(root + collection)).filter(
      !_.getName.matches(".+[0-9][0-9].+"))
    //.filter(f => !f.getParent.contains("multiple-functions"))
  }
  val solverPath = "/Users/krawiec/bs/z3/build/z3"

  for (file <- files) {
    println(file)
    val parseRes = SyGuS16.parseSyGuS16File(file)
    assume(parseRes.isRight)

    val sygusProblem = parseRes match { case Right(t) => t }
    println("SyGuS problem: " + sygusProblem)
    //println( pretty(tree).replace("$colon$colon", "List") )

    // Retrieve the grammar and signature of the function to be synthesized
    val synthTask = ExtractSynthesisTasks(sygusProblem).head  // head

    // Create the Swim grammar from it
    val gr = Grammar.fromMap("Start", synthTask.grammar.toMap)

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
      println("Program " + p + " applied to " + input.unzip._2 +
        " evaluates to " + LIA(p)(inputAsMap))

    ////////////////////////////////////////////////////////////////
    // Run a naive GP with input as the only test 
    println("GP run:")
    val tests = Seq(Test[Map[String, Any], Any](inputAsMap, 0))
    RunExperiment(SimpleGP.Discrete(gr, LIA, tests))

    ////////////////////////////////////////////////////////////////
    // Verifying programs using SMT solver
    println("Verifying programs:")
    // TODO: I'm assuming the free variables are already defined in the Sygus problem, and
    // that all those variables should be later used to form a counterexample. None of these
    // is true in general. 
    val solver = Solver(solverPath, verbose = false)
    val fv = sygusProblem.cmds.collect { case v: VarDeclCmd => v }
    //      val getValueCommand = f"(get-value (${arguments.unzip._1.mkString(" ")}))"
    val getValueCommand = f"(get-value (${fv.map(_.sym).mkString(" ")}))"

    for (p <- progs) {
      // Prepare input to the solver
      val verificationProblem = SMTLIBFormatter.verify(sygusProblem, p)
      // Run the solver:
      val (_, res) = solver.solve(verificationProblem, getValueCommand)
      if (res.isDefined) {
        val cexample = GetValueParser(res.get)
        // IMPORTANT: To run a program on the counterexample, need to rename the values of variables
        // IMPORTANT: This assumes that the free variables defined in the problem correspond one-to-one 
        // (order-preserving) to the arguments of synthesized function.
        val cexampleRenamed = input.unzip._1.zip(cexample.unzip._2)
        println("Counterexample: " + cexampleRenamed)
        val output = LIA.apply(p)(cexampleRenamed.toMap)

        val checkOnTestCmd = SMTLIBFormatter.checkOnInput(sygusProblem, cexample.toMap, output)
        solver.solve(checkOnTestCmd)
      } else {
        println("Correct program found")
      }
    }
  }
}

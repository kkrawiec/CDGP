package tests.other

import cdgp._
import cdgp.solvers.Dreal3
import fuel.util.{Options, Rng}
import swim.Grammar
import swim.tree.{CodeFactory, Op}
import sygus.RealSortExpr
import tests.Global

import scala.collection.mutable


object RealsExperiments extends App {
  implicit val rng = Rng(Options("--seed 0"))

  def generateRandomPrograms(grammar: Grammar, num: Int = 100): Seq[Op] = {
    val cf = new CodeFactory(grammar, stoppingDepth = 6, maxDepth = 9)
    for (i <- 0 until num) yield cf.randomProgram
  }

  val NUM_PROGRAMS = 100

  val vars = List(("a", RealSortExpr()), ("b", RealSortExpr()))
  val synthTask = SygusSynthTask("f", vars, RealSortExpr(), "NRA")
  val programs = generateRandomPrograms(synthTask.getSwimGrammar(rng), NUM_PROGRAMS)


  println("-" * 100)
  println("dReal3")
  println("-" * 100)
  val statsDreal3 = mutable.Map[String, Int]()
  val solverDreal3 = SolverFromScript("/home/iwob/Programy/dReal/dReal3", args="")
  val startDreal3 = System.currentTimeMillis()
  programs.foreach { p =>
    println(p)
    val query = Dreal3.verificationQuery(p, synthTask)
    println("QUERY:\n" + query)
    val out = solverDreal3.apply(query)
    println("OUT:" + out)
    val res = Dreal3.parseResult(out)
    println("RES: " + res)
    statsDreal3.put(res._1, statsDreal3.getOrElse(res._1, 0) + 1)
  }
  val timeDreal3 = System.currentTimeMillis() - startDreal3



  println("\n")
  println("-" * 100)
  println("Z3")
  println("-" * 100)
  val statsZ3 = mutable.Map[String, Int]()
  val solverZ3 = SolverFromScript(Global.solverPath, args=SolverFromScript.ARGS_Z3)
  val startZ3 = System.currentTimeMillis()
  programs.foreach { p =>
    println(p)
    val code = SMTLIBFormatter.apply(p)
    val query =
      s"""(set-logic QF_NRA)
        |${SMTLIBFormatter.produceVarDeclsForSynthFunArgs(synthTask)}
        |(assert (>= $code 0))
        |(check-sat)
        |(get-model)
      """.stripMargin
    val res = solverZ3.solve(new PlainQuery(query))
    statsZ3.put(res._1, statsZ3.getOrElse(res._1, 0) + 1)
    println("RES: " + res)
  }
  val timeZ3 = System.currentTimeMillis() - startZ3



  val p = 9
  println("\n\nSTATISTICS:")
  println("\nDecisions:")
  println("dReal3:".padTo(p, ' ') + statsDreal3)
  println("Z3:".padTo(p, ' ') + statsZ3)
  println("\nRuntime [ms]:")
  println("dReal3:".padTo(p, ' ') + timeDreal3)
  println("Z3:".padTo(p, ' ') + timeZ3)
}

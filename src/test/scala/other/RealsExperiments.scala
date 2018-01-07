package scala.other

import cdgp._
import cdgp.solvers.Dreal3
import fuel.util.{Options, Rng}
import swim.Grammar
import swim.tree.{CodeFactory, Op}
import sygus.{IntSortExpr, RealSortExpr}


object RealsExperiments extends App {
  implicit val rng = Rng(Options("--seed 0"))

  def generateRandomPrograms(grammar: Grammar, num: Int = 100): Seq[Op] = {
    val cf = new CodeFactory(grammar, stoppingDepth = 4, maxDepth = 8)
    for (i <- 0 until num) yield cf.randomProgram
  }

  // TODO: test dReal3 against Z3
  val path = "/home/iwob/Programy/dReal3/dReal3"
  val solver = SolverStdin(path)

  val vars = List(("a", RealSortExpr()), ("b", RealSortExpr()))
  val synthTask = SygusSynthTask("f", vars, RealSortExpr(), "NRA")
  val programs = generateRandomPrograms(synthTask.getSwimGrammar(rng))


  println("-" * 100)
  println("dReal3")
  println("-" * 100)
  programs.foreach { p =>
    println(p)
    val query = Dreal3.verificationQuery(p, synthTask)
    println("res")
  }


  println("-" * 100)
  println("Z3")
  println("-" * 100)
  programs.foreach { p =>
    println(p)
  }

}

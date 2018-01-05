package scala.other

import cdgp._
import fuel.util.{Options, Rng}
import swim.tree.{CodeFactory, Op}


object RealsExperiments extends App {
  implicit val rng = Rng(Options("--seed 0"))

  def generateRandomPrograms(synthTask: SygusSynthTask): Seq[Op] = {
    val cf = new CodeFactory(synthTask.getSwimGrammar(rng), stoppingDepth = 4, maxDepth = 8)
    for (i <- 0 until 100) yield cf.randomProgram
  }

  // TODO: test dReal3 against Z3
  val path = "/home/iwob/Programy/dReal3/dReal3"
  val solver = SolverStdin(path)

  //val programs = generateRandomPrograms()

}

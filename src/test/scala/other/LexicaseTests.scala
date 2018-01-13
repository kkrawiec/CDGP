package scala.other

import cdgp._
import fuel.core.StatePop
import fuel.func.Selection
import fuel.util.{Options, Rng}
import swim.Grammar
import swim.eval.LexicaseSelection01
import swim.tree.{CodeFactory, Op}
import sygus.IntSortExpr


object LexicaseTests extends App {
  implicit val rng = Rng(Options("--seed 0"))

  def generateRandomPop(grammar: Grammar, num: Int = 500): StatePop[(Op, Seq[Int])] = {
    val cf = new CodeFactory(grammar, stoppingDepth = 6, maxDepth = 9)
    val progs = for (i <- 0 until num) yield cf.randomProgram
    StatePop(progs.map{ p =>
      val f = for (i <- 0 until 100) yield rng.nextInt(2)
      (p, f)
    })
  }

  def testSelection(pop: StatePop[(Op, scala.Seq[Int])], selection: Selection[Op, Seq[Int]]): Unit = {
    val start = System.currentTimeMillis()
    for (i <- 0 until 500) {
      val c = selection(pop)
      println("Chosen el: " + c)
    }
    println("Time [ms]: " + (System.currentTimeMillis() - start))
  }

  val NUM_PROGRAMS = 100
  val gr = SygusUtils.getSwimGrammar(SygusUtils.defaultGrammar("LIA", Seq(), IntSortExpr()))
  val pop = generateRandomPop(gr, NUM_PROGRAMS)

  println("SELECTION: lex01")
  val lex01 = new LexicaseSelection01[Op, Seq[Int]]()
  testSelection(pop, lex01)

}

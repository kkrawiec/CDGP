package scala.other

import cdgp._
import fuel.core.StatePop
import fuel.func.Selection
import fuel.util.{Options, Rng}
import swim.Grammar
import swim.eval.{EpsLexicaseSelection, LexicaseSelection, LexicaseSelection01}
import swim.tree.{CodeFactory, Op}
import sygus.IntSortExpr


object LexicaseTests extends App {
  implicit val rng = Rng(Options("--seed 0"))

  def generateRandomPop(grammar: Grammar, num: Int = 500): StatePop[(Op, Seq[Int])] = {
    val cf = new CodeFactory(grammar, stoppingDepth = 6, maxDepth = 9)
    val progs = for (i <- 0 until num) yield cf.randomProgram
    StatePop(progs.map{ p =>
      val f = for (i <- 0 until 200) yield rng.nextInt(2)
      (p, f)
    })
  }

  def generateRandomPopRegression(grammar: Grammar, num: Int = 500): StatePop[(Op, Seq[Double])] = {
    val cf = new CodeFactory(grammar, stoppingDepth = 6, maxDepth = 9)
    val progs = for (i <- 0 until num) yield cf.randomProgram
    StatePop(progs.map{ p =>
      val f = for (i <- 0 until 200) yield rng.nextDouble()
      (p, f)
    })
  }

  def testSelection[E](pop: StatePop[(Op, E)], selection: Selection[Op, E]): Long = {
    val start = System.currentTimeMillis()
    for (i <- 0 until 500) {
      // println(s"Selection #$i")
      val c = selection(pop)
      // println("Chosen el: " + c)
    }
    val d = System.currentTimeMillis() - start
    println("Time [ms]: " + d)
    d
  }

  def testSelectionEpsLexicase[E <: Seq[Double]](pop: StatePop[(Op, E)], selection: EpsLexicaseSelection[Op, E]): Long = {
    val start = System.currentTimeMillis()
    // val epsForTests = EpsLexicaseSelection.medianAbsDev(pop)
    val epsForTests = pop.head._2.map{ _ => 0.1 }.toVector
    for (i <- 0 until 500) {
      // println(s"Selection #$i")
      val c = selection(pop, epsForTests)
      // println("Chosen el: " + c)
    }
    val d = System.currentTimeMillis() - start
    println("Time [ms]: " + d)
    d
  }

  val NUM_PROGRAMS = 10000
  val gr = SygusUtils.getSwimGrammar(SygusUtils.defaultGrammar("LIA", Seq(), IntSortExpr()))
  val pop = generateRandomPop(gr, NUM_PROGRAMS)
  val popRegr = generateRandomPopRegression(gr, NUM_PROGRAMS)

  println("SELECTION: lex01")
  val lex01 = new LexicaseSelection01[Op, Seq[Int]]()
  testSelection(pop, lex01)

  println("SELECTION: lexSel")
  val lexSel = new LexicaseSelection[Op, Int](Ordering[Int])
  testSelection(pop, lexSel)

  println("-" * 100)
  println(" " * 25 + "REGRESSION")
  println("-" * 100)

  println("SELECTION: lexSel")
  val lexSel2 = new LexicaseSelection[Op, Double](Ordering[Double])
  testSelection(popRegr, lexSel2)

  println("SELECTION: epsLex")
  val epsLex = new EpsLexicaseSelection[Op, Seq[Double]]
  val times = 0.until(10).map{ _ => testSelectionEpsLexicase(popRegr, epsLex) }
  println("Avg of 10 runs [ms]: " + (times.sum / times.size.toDouble))
}

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

  def generateRandomPop(grammar: Grammar, numProgs: Int = 500, numTests: Int = 200): StatePop[(Op, Seq[Int])] = {
    val cf = new CodeFactory(grammar, stoppingDepth = 1, maxDepth = 3)
    val progs = for (i <- 0 until numProgs) yield cf.randomProgram
    StatePop(progs.map{ p =>
      val f = for (i <- 0 until numTests) yield rng.nextInt(2)
      (p, f)
    })
  }

  def generateRandomPopRegression(grammar: Grammar, numProgs: Int = 500, numTests: Int = 200): StatePop[(Op, Seq[Double])] = {
    val cf = new CodeFactory(grammar, stoppingDepth = 1, maxDepth = 3)
    val progs = for (i <- 0 until numProgs) yield cf.randomProgram
    StatePop(progs.map{ p =>
      val f = for (i <- 0 until numTests) yield rng.nextDouble()
      (p, f)
    })
  }

  def testSelection[E](pop: StatePop[(Op, E)], selection: Selection[Op, E]): Long = {
    val start = System.currentTimeMillis()
    for (i <- 0 until 500) {
      val c = selection(pop)
      // println("Chosen el: " + c)
    }
    val d = System.currentTimeMillis() - start
    println("Time [ms]: " + d)
    d
  }

  def testSelectionEpsLexicase[E <: Seq[Double]](pop: StatePop[(Op, E)], selection: EpsLexicaseSelection[Op, E]): Long = {
    val start = System.currentTimeMillis()
    for (i <- 0 until 500) {
      val c = selection(pop)
      println("Chosen el: " + c)
    }
    val d = System.currentTimeMillis() - start
    println("Time [ms]: " + d)
    d
  }

  def printPop(pop: StatePop[(Op, Seq[Double])]): Unit = {
    pop.foreach{ p =>
      println(s"${p._1}".padTo(50, ' ') + s"${p._2}")
    }
  }


  def readableExperiment(): Unit = {
    println("\n\n")
    println("Tests on small example")
    val pop = generateRandomPopRegression(gr, 5, 5)
    println("Population:")
    printPop(pop)

    val epsForTests = EpsLexicaseSelection.medianAbsDev(pop)
    println(s"\nepsForTests:\n$epsForTests\n")
    val selection = new EpsLexicaseSelection[Op, Seq[Double]](epsForTests)

    for (i <- 0 until 10) {
      val c = selection(pop)
      //Add this in appropriate line in EpsLexicase: println(s"Shuffle: $t")
      println("Chosen el: " + c)
    }
  }




  val NUM_PROGRAMS = 10000
  val vars = Seq("a", "b", "c", "d", "e").map{ s => (s, IntSortExpr()) }
  val gr = SygusUtils.getSwimGrammar(SygusUtils.defaultGrammar("LIA", vars, IntSortExpr()))
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
  val epsLex = EpsLexicaseSelection(popRegr)
  testSelectionEpsLexicase(popRegr, epsLex)
  // val times = 0.until(10).map{ _ => testSelectionEpsLexicase(popRegr, epsLex) }
  // println("Avg of 10 runs [ms]: " + (times.sum / times.size.toDouble))

  readableExperiment()
}

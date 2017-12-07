package tests

import cdgp.{FInt, FIntOrdering, FSeqInt, FSeqIntOrdering}
import fuel.core.StatePop
import fuel.func.TournamentSelection
import fuel.util.{Options, Rng}
import org.junit.Test
import org.junit.Assert._
import swim.tree.Op


final class TestCDGPAlgorithm {
  val ordFInt = FIntOrdering
  val ordFSeqInt = FSeqIntOrdering
  implicit val rng = Rng(Options("--seed 0"))
  @Test
  def test_FInt(): Unit = {
    val t0_20 = FInt(true, 0, 20)
    val t0_50 = FInt(true, 0, 50)
    val t0_90 = FInt(true, 0, 90)
    val t1_20 = FInt(true, 1, 20)
    val t1_50 = FInt(true, 1, 50)
    val t1_90 = FInt(true, 1, 90)
    val f0_20 = FInt(false, 0, 20)
    val f0_50 = FInt(false, 0, 50)
    val f0_90 = FInt(false, 0, 90)
    val f1_20 = FInt(false, 1, 20)
    val f1_50 = FInt(false, 1, 50)
    val f1_90 = FInt(false, 1, 90)
    assertEquals(1,  ordFInt.compare(t0_50, t0_20))
    assertEquals(0,  ordFInt.compare(t0_50, t0_50))
    assertEquals(-1, ordFInt.compare(t0_50, t0_90))
    assertEquals(1,  ordFInt.compare(t1_50, t0_50))
    assertEquals(-1, ordFInt.compare(t0_50, t1_50))
    assertEquals(-1, ordFInt.compare(t1_20, t1_50))
    assertEquals(0,  ordFInt.compare(t1_50, t1_50))
    assertEquals(1,  ordFInt.compare(t1_90, t1_50))

    assertEquals(-1, ordFInt.compare(t0_50, f0_20))
    assertEquals(-1, ordFInt.compare(t1_50, f0_50))
    assertEquals(1,  ordFInt.compare(f1_50, t1_50))
    assertEquals(1,  ordFInt.compare(f1_90, t1_50))
    assertEquals(-1, ordFInt.compare(t1_90, f0_90))

    assertEquals(1,  ordFInt.compare(f0_50, f0_20))
    assertEquals(0,  ordFInt.compare(f0_50, f0_50))
    assertEquals(-1, ordFInt.compare(f0_50, f0_90))
    assertEquals(1,  ordFInt.compare(f1_50, f0_50))
    assertEquals(-1, ordFInt.compare(f0_20, f1_50))
    assertEquals(-1, ordFInt.compare(f1_20, f1_50))
    assertEquals(0,  ordFInt.compare(f1_50, f1_50))
    assertEquals(1,  ordFInt.compare(f1_90, f1_50))
  }

  @Test
  def test_FInt_Tournament(): Unit = {
    // No assertions in this test, because TournamentSelection is with replacement
    // and it is possible that the same element is chosen several times.
    // This serves rather as a sanity test and code example.
    def s(fit: Int, size: Int) = (Op('nt, "a"), FInt(false, fit, size))
    def r() = (Op('nt, "a"), FInt(false, 1+rng.nextInt(2), 21+rng.nextInt(40)))
    def generatePop() = StatePop(List(r(), r(), r(), r(), s(1, 20)))
    1.to(20).foreach { _ =>
      val pop = generatePop()
      val handMin = pop.map(_._2.progSize).min(Ordering[Int])
      println(pop)
      println("sortBy: " + pop.sortBy(_._2)(ordFInt))
      println("minBy: " + pop.minBy(_._2)(ordFInt))
      val x = new TournamentSelection(ordFInt, 5)(rng)(pop)
      println("Selected: " + x)
    }
  }

  @Test
  def test_FSeqInt(): Unit = {
    val t0_20 = FSeqInt(true, Seq(0), 20)
    val t0_50 = FSeqInt(true, Seq(0), 50)
    val t0_90 = FSeqInt(true, Seq(0), 90)
    val t1_20 = FSeqInt(true, Seq(1), 20)
    val t1_50 = FSeqInt(true, Seq(1), 50)
    val t1_90 = FSeqInt(true, Seq(1), 90)
    val f0_20 = FSeqInt(false, Seq(0), 20)
    val f0_50 = FSeqInt(false, Seq(0), 50)
    val f0_90 = FSeqInt(false, Seq(0), 90)
    val f1_20 = FSeqInt(false, Seq(1), 20)
    val f1_50 = FSeqInt(false, Seq(1), 50)
    val f1_90 = FSeqInt(false, Seq(1), 90)
    val longer = FSeqInt(false, Seq(0, 0), 90)
    assertEquals(1,  ordFSeqInt.compare(t0_50, t0_20))
    assertEquals(0,  ordFSeqInt.compare(t0_50, t0_50))
    assertEquals(-1, ordFSeqInt.compare(t0_50, t0_90))
    assertEquals(1,  ordFSeqInt.compare(t1_50, t0_50))
    assertEquals(-1, ordFSeqInt.compare(t0_50, t1_50))
    assertEquals(-1, ordFSeqInt.compare(t1_20, t1_50))
    assertEquals(0,  ordFSeqInt.compare(t1_50, t1_50))
    assertEquals(1,  ordFSeqInt.compare(t1_90, t1_50))

    assertEquals(-1, ordFSeqInt.compare(t0_50, f0_20))
    assertEquals(-1, ordFSeqInt.compare(t1_50, f0_50))
    assertEquals(1,  ordFSeqInt.compare(f1_50, t1_50))
    assertEquals(1,  ordFSeqInt.compare(f1_90, t1_50))
    assertEquals(-1, ordFSeqInt.compare(t1_90, f0_90))

    assertEquals(1,  ordFSeqInt.compare(f0_50, f0_20))
    assertEquals(0,  ordFSeqInt.compare(f0_50, f0_50))
    assertEquals(-1, ordFSeqInt.compare(f0_50, f0_90))
    assertEquals(1,  ordFSeqInt.compare(f1_50, f0_50))
    assertEquals(-1, ordFSeqInt.compare(f0_20, f1_50))
    assertEquals(-1, ordFSeqInt.compare(f1_20, f1_50))
    assertEquals(0,  ordFSeqInt.compare(f1_50, f1_50))
    assertEquals(1,  ordFSeqInt.compare(f1_90, f1_50))

    assertEquals(1,  ordFSeqInt.compare(f1_90, longer))
    assertEquals(-1,  ordFSeqInt.compare(longer, f0_20))
    assertEquals(0,  ordFSeqInt.compare(longer, longer))
  }
}

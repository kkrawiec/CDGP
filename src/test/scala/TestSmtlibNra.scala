package tests

import cdgp._
import fuel.util.{CollectorStdout, Options, Rng}
import org.junit.Assert._
import org.junit.Test
import swim.tree.Op

final class TestSmtlibNra {
  implicit val emptyOpt = Options(s"--selection lexicase --evolutionMode generational ${Global.solverConfig}")
  implicit val coll = CollectorStdout(emptyOpt)
  implicit val rng = Rng(emptyOpt)
  println("Creating solver.")
  val solver = SolverManager(emptyOpt, coll)

  @Test
  def test_verifyNraBenchmarks(): Unit = {
    def verify(path: String, correct: String, expected: String = "unsat") {
      println(s"Problem: $path")
      val problem = LoadSygusBenchmark(path)
      val templateVerification = new TemplateVerification(SygusProblemData(problem))
      val query = templateVerification(SMTLIBFormatter.smtlibToOp(correct))
//       println(s"Query:\n$query")
      val start = System.currentTimeMillis()
      val (dec, model) = solver.executeQuery(query)
      println("Time: " + (System.currentTimeMillis() - start) + " [ms]")
      println(s"Result: ($dec, $model)\n")
      assertEquals(expected, dec)
    }
    verify("resources/benchmarks_phd/cdsr/withNoise/gravity_500.sl", """(/ (* m1 m2 0.00000000006674) (* r r))""")
    verify("resources/benchmarks_phd/cdsr/withNoise/keijzer5_500.sl", """(/ (* 30.0 x z) (* (- x 10.0) (* y y)))""")
    verify("resources/benchmarks_phd/cdsr/withNoise/keijzer12_500.sl", """(+ (* x x x x) (- (* x x x)) (* y y 0.5) (- y))""")
    verify("resources/benchmarks_phd/cdsr/withNoise/keijzer14_500.sl", """(/ 8.0 (+ 2.0 (* x x) (* y y)))""")
    verify("resources/benchmarks_phd/cdsr/withNoise/keijzer15_500.sl", """(+ (* x x x 0.2) (* y y y 0.5) (- y) (- x))""")
    verify("resources/benchmarks_phd/cdsr/withNoise/nguyen1_500.sl", """(+ (* x x x) (* x x) x)""")
    verify("resources/benchmarks_phd/cdsr/withNoise/nguyen3_500.sl", """(+ (* x x x x x) (* x x x x) (* x x x) (* x x) x)""")
    verify("resources/benchmarks_phd/cdsr/withNoise/nguyen4_500.sl", """(+ (* x x x x x x) (* x x x x x) (* x x x x) (* x x x) (* x x) x)""")
    verify("resources/benchmarks_phd/cdsr/withNoise/pagie1_500.sl", """(+ (/ 1.0 (+ 1.0 (/ 1.0 (* x x x x))))  (/ 1.0 (+ 1.0 (/ 1.0 (* y y y y)))) )""")
    verify("resources/benchmarks_phd/cdsr/withNoise/resistance_par2_500.sl", """(/ (* r1 r2) (+ r1 r2))""")
    verify("resources/benchmarks_phd/cdsr/withNoise/resistance_par3_500.sl", """(/ (* r1 r2 r3) (+ (* r1 r2) (* r2 r3) (* r1 r3)))""")  // error in benchmark
  }

}

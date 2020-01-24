package misc.benchmarks

import java.io.{BufferedWriter, File, FileWriter}

import fuel.util.TRandom
import misc.{Benchmark, Range}

import scala.util.Random


object Utils {
  val rng = Random

  def generateTestU(numVars: Int, fun: Seq[Double] => Double,
                    minDouble: Double, maxDouble: Double): (Seq[Double], Double) = {
    def rngDouble() = minDouble + rng.nextDouble() * (maxDouble+1-minDouble)
    val in = 0.until(numVars).map{ i => BigDecimal(rngDouble()).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble }
    val out = fun(in)
    (in, out)
  }

  def generateTestsU(numVars: Int, numTests: Int, fun: Seq[Double] => Double,
                     minDouble: Double, maxDouble: Double): Seq[(Seq[Double], Double)] = {
    0.until(numTests).map{ _ => generateTestU(numVars, fun, minDouble, maxDouble) }
  }

  def rangesGeqZero01(vars: String*): Seq[Range] = vars.map(x => Range(x, lb=Some(0.01), lbSign = ">="))
  def rangesGtZero(vars: String*): Seq[Range] = vars.map(x => Range(x, lb=Some(0.0), lbSign = ">"))
  def rangesLeqZero01(vars: String*): Seq[Range] = vars.map(x => Range(x, ub=Some(-0.01), ubSign = "<="))
  def rangesLtZero(vars: String*): Seq[Range] = vars.map(x => Range(x, ub=Some(0.0), ubSign = "<"))

  def saveFile(path: String, text: String): Unit = {
    try {
      val file = new File(path)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(text)
      bw.close()
    }
    catch {
      case e: Exception => println(s"Error while saving file $path.\nError:\n${e.getMessage}")
    }
  }

  def ensureDir(path: String): Unit = {
    val dir = new File(path)
    if (dir.exists())
      dir.delete()
    dir.mkdir()
  }
}


object NoiseAdder {
  /**
    * Adds a noise from a normal distribution to the output of the test. Average of this distribution is
    * the exact value, and the standard deviation is the provided percent of that value.
    */
  def noiseNormalPercentB(b: Benchmark, percentY: Double, percentX: Double = 0.0)
                         (implicit rng: TRandom): Benchmark = {
    val newTests = noiseNormalPercent(b.tests, percentY, percentX)
    Benchmark(b.funName, b.vars, b.props, newTests)
  }

  /**
    * Adds a noise from a normal distribution to the output of the test. Average of this distribution is
    * the exact value, and the standard deviation is the provided percent of that value.
    */
  def noiseNormalPercent(tests: Seq[(Seq[Double], Double)], percentY: Double, percentX: Double = 0.0)
                        (implicit rng: TRandom): Seq[(Seq[Double], Double)] = {
    def newValue(v: Double, p: Double): Double = v + rng.nextGaussian() * p * v
    tests.map{ case (inputs, output) =>
        val newInputs = if (percentX == 0.0) inputs else inputs.map(newValue(_, percentX))
        val newOutput = newValue(output, percentY)
      (newInputs, newOutput)
    }
  }
}
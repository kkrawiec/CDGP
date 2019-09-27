package cdgp

import fuel.util.{Collector, Options, TRandom}
import scala.collection.mutable


/**
  * Manages the set of test cases during evolution run. If test output is None, then
  * the desired output for test's input is not known yet or can assume many possible values.
  *
  * @param newTests Set of counterexamples collected from the current generation. To be reset after each iteration.
  */
class TestsManagerCDGP[I,O](val tests: mutable.ArrayBuffer[(I, Option[O])],
                            val newTests: mutable.ArrayBuffer[(I, Option[O])],
                            val testsHistory: Boolean = false,
                            val printAddedTests: Boolean = false,
                            val saveTests: Boolean = false) {
  private var flushNo = 0
  def getNumFlushes: Int = flushNo

  /**
    * Stores inputs of the tests used so that duplicates can be quickly detected when needed.
    * Tests from newTests are not taken into account.
    */
  private val keysIndex = mutable.HashSet[I](tests.map(_._1):_*)

  /** Stores the number of tests added after each use of flushHelpers. **/
  val history: mutable.Map[Int, Int] = mutable.Map[Int, Int]()

  /** Returns collected 'old' tests. */
  def getTests(): Seq[(I, Option[O])] = tests
  /** Returns both collected 'new' and 'old' tests. */
  def getAllCollectedTests: List[(I, Option[O])] = tests.toList ++: newTests.toList
  def getNumberOfTests: Int = tests.size
  def getNumberOfKnownOutputs: Int = tests.count{ case (in, out) => out.isDefined}
  def getNumberOfUnknownOutputs: Int = getNumberOfTests - getNumberOfKnownOutputs

  def addNewTests(ts:Seq[(I, Option[O])], allowInputDuplicates: Boolean = true, allowTestDuplicates: Boolean = false) {
    ts.foreach(addNewTest(_, allowInputDuplicates, allowTestDuplicates))
  }
  /** This method takes into account only newTests and inputs of the tests. Duplicates of already accepted tests are not checked. **/
  def addNewTest(t: (I, Option[O]), allowInputDuplicates: Boolean = true, allowTestDuplicates: Boolean = false) {
    //println("** Trying to add new test: " + t)
    if (allowInputDuplicates || (!keysIndex.contains(t._1) && !newTests.exists(_._1 == t._1))) {
      if (allowTestDuplicates || (!keysIndex.contains(t._1) && !newTests.contains(t)))
        newTests.append(t)
    }
  }

  /** Updates test for the given index. **/
  def updateTest(index: Int, t: (I, Option[O])) {
    //println(s"** Updated test #$index: $t")
    val prevKey = tests(index)._1
    tests(index) = t
    // updating keyIndex
    if (!tests.exists(_._1.equals(prevKey)))
      keysIndex.remove(prevKey)  // key is not present, remove it
    keysIndex += t._1
  }

  /**
    * Returns a list of tests with n first element dropped.
    */
  def dropFromTests(n: Int): Seq[(I, Option[O])] = {
    if (tests.size == n) Seq() else tests.drop(n).toList
  }

  /**
    * Moves elements from newTests to a global tests pool, and prepares manager for the next iteration
    * by clearing newTests.
    */
  def flushHelpers() {
    for (test <- newTests) {
      // It's not checked whether a duplicate is added or not, because such check was already conducted
      // when the test was added to the newTests list.
      tests.append(test)  // append test
      keysIndex.add(test._1)
      if (printAddedTests) println(s"Added test: $test")
    }
    if (testsHistory && newTests.nonEmpty)
      history.put(flushNo, newTests.size)
    newTests.clear
    flushNo += 1
  }


  /**
    * Saves tests-related info and statistics in the collector.
    */
  def reportData(coll: Collector, prefix: String = "tests") {
    coll.set(s"$prefix.total", tests.size)
    coll.set(s"$prefix.testsHistory", history.toList.sorted.mkString(", "))
    coll.set(s"$prefix.totalKnownOutputs", getNumberOfKnownOutputs)
    coll.set(s"$prefix.totalUnknownOutputs", getNumberOfUnknownOutputs)
    if (saveTests)
      coll.set(s"$prefix.collected", tests.toString)
  }
}


object TestsManagerCDGP {
  def apply[I,O](testsHistory: Boolean = false, printAddedTests: Boolean = false, saveTests: Boolean = false)
                (implicit opt: Options, rng: TRandom): TestsManagerCDGP[I,O] = {
    val tests = mutable.ArrayBuffer[(I, Option[O])]()
    val newTests = mutable.ArrayBuffer[(I, Option[O])]()
    new TestsManagerCDGP(tests, newTests, testsHistory, printAddedTests, saveTests)
  }
  def apply[I,O]()(implicit opt: Options, rng: TRandom): TestsManagerCDGP[I,O] = {
    val testsHistory = opt('logTestsHistory, false)
    val printAddedTests = opt('printTests, false)
    val saveTests = opt('saveTests, false)
    TestsManagerCDGP(testsHistory, printAddedTests, saveTests)
  }
}




object NoiseAdderStdDev {
  /**
    * Returns a new instance of the tests manager with noise added to the tests. Noise can be added both
    * to the dependent variable (noiseY) and independent variables (noiseX). Noise on the certain variable is
    * generated from the normal distribution with the mean 0 and with the standard deviation equal to
    * delta * standard deviation of the variable in question (computed from the sample).
    *
    * @param manager tests manager.
    * @param deltaY Factor of the noise on the dependent variable. 0.0 means no noise.
    * @param deltaX Factor of the noise on the independent variables. 0.0 means no noise.
    * @return new tests manager instance with noise added to tests.
    */
  def apply(manager: TestsManagerCDGP[Map[String, Any], Any], deltaY: Double, deltaX: Double = 0.0)
           (implicit rng: TRandom): TestsManagerCDGP[Map[String, Any], Any] = {
    if ((manager.tests.isEmpty && manager.newTests.isEmpty) || (deltaX == 0.0 && deltaY == 0.0)) {
      manager  // if no tests or both delta=0 then return manager
    }
    else {
      val allTestsSet = manager.newTests.toSet ++ manager.tests.toSet
      val keys = if (manager.tests.nonEmpty) manager.tests.head._1.keys else manager.newTests.head._1.keys

      // Create a vector of std deviations for each column
      val stdDevs = keys.map{ k: String => stdDevForInputVar(allTestsSet, k) }.toSeq
      val stdDevOut = stdDevForOutput(allTestsSet)

      // Randomizing tests
      val tests2 = randomizeTests(manager.tests, stdDevs, stdDevOut, deltaY=deltaY, deltaX=deltaX)
      val newTests2 = randomizeTests(manager.newTests, stdDevs, stdDevOut, deltaY=deltaY, deltaX=deltaX)

      // Adding tests
      val tests3 = mutable.ArrayBuffer[(Map[String, Any], Option[Any])]()
      val newTests3 = mutable.ArrayBuffer[(Map[String, Any], Option[Any])]()
      tests2.foreach(t => tests3.append(t))
      newTests2.foreach(t => newTests3 += t)

      // println("Tests before:\n" + manager.getTests().mkString("\n"))
      // println("New tests before:\n" + manager.newTests.mkString("\n"))
      val t = new TestsManagerCDGP[Map[String, Any], Any](tests3, newTests3, manager.testsHistory, manager.printAddedTests, manager.saveTests)
      // println("Tests after:\n" + t.getTests().mkString("\n"))
      // println("New tests after:\n" + t.newTests.mkString("\n"))
      t
    }
  }

  def apply(manager: TestsManagerCDGP[Map[String, Any], Any])
           (implicit rng: TRandom, opt: Options): TestsManagerCDGP[Map[String, Any], Any] = {
    val deltaY = opt('noiseDeltaY, 0.0)
    val deltaX = opt('noiseDeltaX, 0.0)
    apply(manager, deltaY=deltaY, deltaX=deltaX)
  }

  def randomizeTests(tests: Seq[(Map[String, Any], Option[Any])],
                     stdDevs: Seq[Double], stdDevOut: Double,
                     deltaY: Double, deltaX: Double)
                    (implicit rng: TRandom):
  Seq[(Map[String, Any], Option[Any])] = {
    tests.map { case (input, output) =>
      if (output.isEmpty) (input, output)
      else {
        val newInput = input.toSeq.zip(stdDevs).map{case ((k, value), dev) =>
          (k, addNoise(value.asInstanceOf[Double], dev, deltaX))
        }.toMap
        val newOutput = Some(addNoise(output.get.asInstanceOf[Double], stdDevOut, deltaY))
        (newInput, newOutput)
      }
    }
  }

  /** Standard deviation for a set of tests where tests without defined output are ignored. */
  def stdDevForInputVar(tests: Set[(Map[String, Any], Option[Any])], key: String): Double = {
    val col = tests.filter(_._2.isDefined).map(_._1(key)).toSeq.asInstanceOf[Seq[Double]]
    Tools.stddev(col)
  }

  /** Standard deviation for a set of tests where tests without defined output are ignored. */
  def stdDevForOutput(tests: Set[(Map[String, Any], Option[Any])]): Double = {
    val col = tests.filter(_._2.isDefined).map(_._2.get).toSeq.asInstanceOf[Seq[Double]]
    Tools.stddev(col)
  }

  def addNoise(x: Double, stdDev: Double, delta: Double)(implicit rng: TRandom): Double = {
    x + rng.nextGaussian() * stdDev * delta
  }
}
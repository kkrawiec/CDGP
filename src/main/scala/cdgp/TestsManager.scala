package cdgp

import fuel.util.Collector

import scala.collection.mutable


/**
  * Manages the set of test cases during evolution run. If test output is None, then
  * the desired output for test's input is not known yet or can assume many possible values.
  */
class TestsManagerCDGP[I,O](val tests: mutable.LinkedHashMap[I, Option[O]], val testsHistory: Boolean = false,
                            val printAddedTests: Boolean = false, val saveTests: Boolean = false) {
  // Set of counterexamples collected from the current generation. To be reseted after each iteration.
  val newTests: mutable.Set[(I, Option[O])] = mutable.Set[(I, Option[O])]()

  private var flushNo = 0
  def getNumFlushes: Int = flushNo
  // Stores the number of tests after each use of flushHelpers.
  val history: mutable.Map[Int, Int] = mutable.Map[Int, Int]()

  def getTests(): List[(I, Option[O])] = {
    tests.toList
  }
  def getNumberOfTests: Int = tests.size
  def getNumberOfKnownOutputs: Int = tests.values.count(_.isDefined)
  def getNumberOfUnknownOutputs: Int = tests.size - getNumberOfKnownOutputs

  def addNewTests(ts:Seq[(I, Option[O])]) { ts.foreach(addNewTest(_)) }

  def addNewTest(t: (I, Option[O])) {
    //println("** Trying to add new test: " + t)
    if (!tests.contains(t._1)) {
      newTests.+=(t)
    }
  }
  def updateTest(t: (I, Option[O])) {
    //println("** Updated test: " + t)
    tests.put(t._1, t._2)
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
    for (test <- newTests)
      if (!tests.contains(test._1)) {
        tests.put(test._1, test._2)
        if (printAddedTests) println("Added test: " + test)
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
  def apply[I,O](testsHistory: Boolean = false, printAddedTests: Boolean = false, saveTests: Boolean = false): TestsManagerCDGP[I,O] = {
    val tests: mutable.LinkedHashMap[I, Option[O]] = mutable.LinkedHashMap[I, Option[O]]()
    new TestsManagerCDGP(tests, testsHistory, printAddedTests, saveTests)
  }
}

object NoiseAdder {
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
  def addNoiseStdDev(manager: TestsManagerCDGP[Seq[Double], Double], deltaY: Double, deltaX: Double = 0.0): TestsManagerCDGP[Seq[Double], Double] = {
    val tests = manager.tests.toSeq

    new TestsManagerCDGP(manager.tests, manager.testsHistory, manager.printAddedTests, manager.saveTests)
  }

  def addNoiseToSeqStdDev(seq: Seq[Double]): Seq[Double] = {
    ??? //val dev = Tools.stddev()
  }
}
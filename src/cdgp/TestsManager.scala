package cdgp

import scala.collection.mutable


/**
  * Manages the set of test cases during evolution run.
  */
class TestsManagerCDGP[I,O](testsHistory: Boolean = true, printAddedTests: Boolean = false) {
  // Set of counterexamples collected along the run.
  // The Option is None if the desired output for a given input is not known yet.
  val tests: mutable.LinkedHashMap[I, Option[O]] = mutable.LinkedHashMap[I, Option[O]]()
  // Set of counterexamples collected from the current generation. To be reseted after each iteration.
  val newTests: mutable.Set[(I, Option[O])] = mutable.Set[(I, Option[O])]()

  private var flushNo = 0
  def getNumFlushes = flushNo
  // Stores the number of tests after each use of flushHelpers.
  val history: mutable.Map[Int, Int] = mutable.Map[Int, Int]()

  def getTests(): List[(I, Option[O])] = {
    tests.toList
  }
  def getNumberOfTests: Int = tests.size
  def getNumberOfKnownOutputs: Int = tests.values.count(_.isDefined)
  def getNumberOfUnknownOutputs: Int = tests.size - getNumberOfKnownOutputs

  def addNewTest(t: (I, Option[O])) {
    //println("** Trying to add new test: " + t)
    if (!tests.contains(t._1)) {
      if (printAddedTests) println("Added test: " + t)
      newTests.+=(t)
    }
  }
  def updateTest(t: (I, Option[O])) {
    //println("** Updated test: " + t)
    tests.put(t._1, t._2)
  }

  /**
    * Moves elements from newTests to a global tests pool, and prepares manager for the next iteration
    * by clearing newTests.
    */
  def flushHelpers() {
    for (test <- newTests)
      if (!tests.contains(test._1)) {
        tests.put(test._1, test._2)
      }
    if (testsHistory && newTests.nonEmpty)
      history.put(flushNo, newTests.size)
    newTests.clear
    flushNo += 1
  }
}

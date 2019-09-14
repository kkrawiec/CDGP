package cdgp

import fuel.util.{Collector, Options, TRandom}
import swim.RecursiveDomain
import swim.tree.{LongerOrMaxPassedOrdering, Op}
import sygus.{IntConst, LiteralTerm, StringConst, Term}



trait Fitness {
  def correct: Boolean
  /**
    * Saves all the fitness-relevant data using the provided collector.
    */
  def saveInColl(coll: Collector): Unit

  /**
    * Total number of tests the solution was evaluated on.
    */
  def totalTests: Int
}


case class FSeqInt(correct: Boolean, value: Seq[Int], progSize: Int)
  extends Seq[Int] with Fitness {
  override def length: Int = value.length
  override def apply(idx: Int) = value(idx)
  override def iterator = value.iterator
  override val totalTests: Int = length

  override def saveInColl(coll: Collector): Unit = {
    val passedTests = if (correct) this.size else this.count(_ == 0)
    val ratio = if (this.isEmpty) 1.0 else passedTests.toDouble / this.size
    val roundedRatio = BigDecimal(ratio).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.setResult("best.passedTests", passedTests)
    coll.setResult("best.numTests", this.size)
    coll.setResult("best.passedTestsRatio", roundedRatio)
    coll.setResult("best.isOptimal", correct)
  }
  override def toString: String = s"Fit($correct, $value, progSize=$progSize)"
}


case class FSeqDouble(correct: Boolean, value: Seq[Double], progSize: Int)
  extends Seq[Double] with Fitness {
  override def length: Int = value.length
  override def apply(idx: Int) = value(idx)
  override def iterator = value.iterator
  override val totalTests: Int = length

  lazy val mse: Double = if (value.isEmpty) 0.0 else Tools.mse(value)

  override def saveInColl(coll: Collector): Unit = {
    //val mseRound = BigDecimal(mse).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.setResult("best.mse", mse)
    coll.setResult("best.isOptimal", correct)
  }
  override def toString: String = s"Fit($correct, $value, progSize=$progSize)"
}


case class FInt(correct: Boolean, value: Int, progSize: Int, override val totalTests: Int) extends Fitness {
  override def saveInColl(coll: Collector): Unit = {
    val passedTests = if (correct) totalTests else totalTests - value
    val ratio = if (totalTests == 0) 1.0 else passedTests.toDouble / totalTests
    val roundedRatio = BigDecimal(ratio).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.setResult("best.passedTests", passedTests)
    coll.setResult("best.numTests", totalTests)
    coll.setResult("best.passedTestsRatio", roundedRatio)
    coll.setResult("best.isOptimal", correct)
  }
  override def toString: String = s"Fit($correct, $value, progSize=$progSize)"
}
object FInt {
  def apply(correct: Boolean, list: Seq[Int], progSize: Int): FInt =
    FInt(correct, list.sum, progSize, list.size)
}


case class FDouble(correct: Boolean, value: Double, progSize: Int) extends Fitness {
  override val totalTests: Int = ???
  override def saveInColl(coll: Collector): Unit = {
    val rounded = BigDecimal(value).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.setResult("best.mse", rounded)
    coll.setResult("best.isOptimal", correct)
  }
  override def toString: String = s"Fit($correct, $value, progSize=$progSize)"
}


object FSeqIntOrdering extends Ordering[FSeqInt] {
  override def compare(a: FSeqInt, b: FSeqInt): Int = {
    val c = if (a.correct && !b.correct) -1
    else if (!a.correct && b.correct) 1
    else LongerOrMaxPassedOrdering.compare(a.value, b.value)
    // lexicographic parsimony pressure
    if (c == 0) a.progSize compare b.progSize
    else c
  }
}
object FSeqDoubleOrderingMSE extends Ordering[FSeqDouble] {
  override def compare(a: FSeqDouble, b: FSeqDouble): Int = {
    val c = if (a.correct && !b.correct) -1
    else if (!a.correct && b.correct) 1
    else a.mse.compareTo(b.mse)
    // lexicographic parsimony pressure
    if (c == 0) a.progSize compare b.progSize
    else c
  }
}
object FIntOrdering extends Ordering[FInt] {
  override def compare(a: FInt, b: FInt): Int = {
    val c = if (a.correct && !b.correct) -1
    else if (!a.correct && b.correct) 1
    else a.value compare b.value
    // lexicographic parsimony pressure
    if (c == 0) a.progSize compare b.progSize
    else c
  }
}
object FDoubleOrdering extends Ordering[FDouble] {
  override def compare(a: FDouble, b: FDouble): Int = {
    val c = if (a.correct && !b.correct) -1
    else if (!a.correct && b.correct) 1
    else a.value compare b.value
    // lexicographic parsimony pressure
    if (c == 0) a.progSize compare b.progSize
    else c
  }
}




//////////////////////////////////////////////////////////////////////////////////////
// EvalFunction
//////////////////////////////////////////////////////////////////////////////////////



abstract class EvalFunction[S, E](val state: State)
                                 (implicit opt: Options, coll: Collector)
  extends Function[S, E] {
  // The types for input and output
  type I = Map[String, Any]
  type O = Any

  // Creating a domain for evaluation by program execution
  lazy val domain: RecursiveDomain[Any, Any] = getDomain(state.sygusData.logic)


  /**
    * Creates a domain, which is used for execution of the programs.
    */
  def getDomain(logic: String): RecursiveDomain[Any, Any] = logic match {
    case "SLIA" | "NIA" | "LIA" | "QF_NIA" | "QF_LIA" | "S" | "QF_S" | "ALL" =>
      DomainSLIA(state.synthTask.argNames, Symbol(state.synthTask.fname), opt("recDepthLimit", 1000))
    case "NRA" | "LRA" | "QF_NRA" | "QF_LRA"=>
      DomainReals(state.synthTask.argNames, Symbol(state.synthTask.fname), opt("recDepthLimit", 1000))
    case _ =>
      throw new Exception(s"Trying to create domain for the unsupported logic: $logic")
  }

  /**
    * Function used to update existing solution-evaluation pair. Used in steady state
    * evolution variant, when the number of tests increases during runtime and older
    * solutions need to be updated.
    */
  def updateEval(s: (S, E)): (S, E)

  /**
    * Computes fitness for a solution. init is a flag specifying that solutions are to
    * be assigned some initial values, and is used mostly in steady state variant to avoid
    * verifying all solutions during the first evaluation phase.
    */
  def apply(s: S): E = apply(s, false)
  def apply(s: S, init: Boolean): E

  /**
    * Assigns a default ("zero") evaluation to the given solution. This function is used for example
    * during initialization of the population in steady state algorithm.
    */
  def defaultValue(s: S): E

  /**
    * A function for checking, if the given solution is optimal.
    */
  val correct: E => Boolean

  /**
    * An ordering used to define the order relation of different fitnesses.
    */
  val ordering: Ordering[E]
}




abstract class EvalCDGP[E, EVecEl](state: StateCDGP)
                                  (implicit opt: Options, coll: Collector)
  extends EvalFunction[Op, E](state) {
  val maxNewTestsPerIter: Int = opt('maxNewTestsPerIter, Int.MaxValue, (x: Int) => x >= 0)
  val testsRatio: Double = opt('testsRatio, 1.0, (x: Double) => x >= 0.0 && x <= 1.0)
  val testsDiff: Option[Int] = opt.getOptionInt("testsDiff")
  val partialConstraintsInFitness: Boolean = opt('partialConstraintsInFitness, false)
  val globalConstraintInFitness: Boolean = opt('globalConstraintInFitness, false)
  val sizeInFitness: Boolean = opt('sizeInFitness, false)

  /** The number of constraints tests prepended to the evaluation vector.*/
  val numberOfConstraintTests: Int = {
    (if (!partialConstraintsInFitness) 0 else state.sygusData.formalConstr.size) +
      (if (sizeInFitness) 1 else 0) +
      (if (globalConstraintInFitness) 1 else 0)
  }


  /** Constructs a vector for fitness elements constructed with the use of the formal verification. */
  def getConstraintsVector(s: Op, passValue: EVecEl, nonpassValue: EVecEl): Seq[EVecEl] = {
    var vector = Seq[EVecEl]()
    if (partialConstraintsInFitness)
      vector = getPartialConstraintsVector(s, passValue, nonpassValue) ++: vector
    if (globalConstraintInFitness)
      vector = getGlobalConstraintsDecision(s, passValue, nonpassValue) +: vector
    if (sizeInFitness)
      vector = getSize(s) +: vector
    vector
  }

  /** Verifies solution on all formal constraints in order to add this info to the fitness vector. */
  def getGlobalConstraintsDecision(s: Op, passValue: EVecEl, nonpassValue: EVecEl): EVecEl = {
    val (dec, _) = state.verify(s)
    if (dec == "unsat") passValue else nonpassValue
  }

  /** Size of the Op converted to the correct unit. */
  def getSize(s: Op): EVecEl

  /** Verifies solution on partial constraints in order to add this info to the fitness vector. */
  def getPartialConstraintsVector(s: Op, passValue: EVecEl, nonpassValue: EVecEl): Seq[EVecEl] = {
    state.sygusData.formalConstr.map{ constr =>
      val template = new TemplateVerification(state.sygusData, false, state.timeout, Some(Seq(constr)))
      val (dec, _) = state.verify(s, template)
      if (dec == "unsat") passValue else nonpassValue
    }
  }

  def extractTestsNormal(eval: Seq[Double]): Seq[Double] = eval.drop(numberOfConstraintTests)
  def extractTestsConstraints(eval: Seq[Double]): Seq[Double] = eval.take(numberOfConstraintTests)

  def handleEvalException(test: (I, Option[O]), s: Op, message: String) {
    val msg = s"Error during evalutation of $s and test $test: $message"
    coll.set("error_evalOnTests", msg)
    println(msg)
  }
}





abstract class EvalCDGPDiscrete[E](state: StateCDGP)
                                  (implicit opt: Options, coll: Collector)
  extends EvalCDGP[E, Int](state) {

  override def getSize(s: Op): Int = s.size

  /**
    * Tests a program on the available tests and returns the vector of 0s (passed test)
    * and 1s (failed test). Depending on the problem will either optimize by executing
    * program directly on the tests, or will have to resort to a solver.
    */
  def evalOnTests(s: Op, tests: Seq[(I, Option[O])]): Seq[Int] = {
    val testsStandard = for (test <- tests) yield { evaluateTest(s, test) }
    if (partialConstraintsInFitness || globalConstraintInFitness || sizeInFitness)
      getConstraintsVector(s, 0, 1) ++: testsStandard
    else
      testsStandard
  }


  /**
    * Computes a fitness data for this test.
    */
  def evaluateTest(s: Op, test: (I, Option[O])): Int = {
    try {
      if (test._2.isDefined)
      // User can define test cases for a problem, in which generally single-answer
      // property does not hold. We will use domain for those cases, since it is more
      // efficient.
        evalTestUsingDomain(s, test)
      else evalTestUsingSolver(s, test)
    }
    catch {
      case _: ExceptionIncorrectOperation => 1
      case e: Throwable => handleEvalException(test, s, e.getMessage); 1
    }
  }


  /**
    * Checks correctness of the program only for the given test.
    * Tests here always have None as the answer, because in general there is no
    * single answer for the problem being solved in 'solver' mode.
    */
  def evalTestUsingSolver(s: Op, test: (I, Option[O])): Int = {
    val testInput: Map[String, Any] = test._1
    val (dec, _) = state.checkIsProgramCorrectForInput(s, testInput)
    if (dec == "sat") 0 else 1
  }


  /**
    * Checks correctness of the program only for the given test.
    * If test has a defined expected answer, then it is compared with the answer
    * obtained by executing the program in the domain simulating semantics of SMT
    * theory.
    * If test don't have defined expected answer, then the program's output is verified
    * by the solver for consistency with the specification. The test will be updated if
    * this output is deemed consistent by the solver.
    *
    * Names of variables in test should be the same as those in the function's invocation.
    * They will be renamed for those in the function's declaration.
    */
  def evalTestUsingDomain(s: Op, test: (I, Option[O])): Int = {
    assert(test._2.isDefined, "Trying to evaluate using the domain a test without defined expected output.")
    val testInput: Map[String, Any] = test._1
    val testOutput: Option[Any] = test._2
    val inputVector = state.synthTask.argNames.map(testInput(_))
    val output = domain(s)(inputVector)
    if (output.isEmpty)
      1  // None means that recurrence depth was exceeded
    else if (testOutput.isDefined) {
      if (output.get == state.convertValue(testOutput.get))
        0
      else
        1
    }
    else {
      // Situation, when the test case has None as the expected output
      // We don't allow such a situation
      throw new Exception("Trying to domain-evaluate a test without defined correct answer!")

      // The code below can be used to try to find the expected output for the test
      //val (dec, _) = checkIsOutputCorrectForInput(s, testInput, output.get)
      //if (dec == "sat")
      //  testsManager.updateTest((testInput, output))
      //if (dec == "sat") 0 else 1
    }
  }

  def fitnessOnlyTestCases: Op => (Boolean, Seq[Int]) =
    (s: Op) => {
      val evalTests = evalOnTests(s, state.testsManager.getTests())
      if (evalTests.sum == 0)
        (true, evalTests)
      else
        (false, evalTests)
    }

  def doVerify(evalTests: Seq[Int]): Boolean = {
    val numPassed = evalTests.count(_ == 0).asInstanceOf[Double]
    if (testsDiff.isDefined)
      numPassed >= evalTests.size - testsDiff.get
    else
      evalTests.isEmpty || (numPassed / evalTests.size) >= testsRatio
  }

  /** Fitness is always computed on the tests that were flushed. */
  def fitnessCDGPGeneral: Op => (Boolean, Seq[Int]) =
    if (state.sygusData.formalInvocations.isEmpty) fitnessOnlyTestCases
    else (s: Op) => {
      val evalTests = evalOnTests(s, state.testsManager.getTests())
      // If the program passes the specified ratio of test cases, it will be verified
      // and a counterexample will be produced (or program will be deemed correct).
      // NOTE: if the program does not pass all test cases, then the probability is high
      // that the produced counterexample will already be in the set of test cases.
      if (!doVerify(evalTests))
        (false, evalTests)
      else {
        val (decision, r) = state.verify(s)
        if (decision == "unsat" && evalTests.sum == 0 && (!(state.sygusData.logic == "SLIA") || evalTests.nonEmpty))
          (true, evalTests) // perfect program found; end of run
        else if (decision == "sat") {
          if (state.testsManager.newTests.size < maxNewTestsPerIter) {
            val newTest = state.createTestFromFailedVerification(r.get)
            if (newTest.isDefined)
              state.testsManager.addNewTest(newTest.get)
          }
          (false, evalTests)
        }
        else {
          // The 'unknown' or 'timeout' solver's decision. Program potentially may be the optimal
          // solution, but solver is not able to verify this. We proceed by adding no new tests
          // and treating the program as incorrect.
          (false, evalTests)
        }
      }
    }
}



class EvalCDGPSeqInt(state: StateCDGP)
                    (implicit opt: Options, coll: Collector)
  extends EvalCDGPDiscrete[FSeqInt](state) {
  override def apply(s: Op, init: Boolean): FSeqInt = {
    if (init) {
      val e = evalOnTests(s, state.testsManager.getTests())
      FSeqInt(false, e, s.size)
    }
    else {
      val (isPerfect, eval) = fitnessCDGPGeneral(s)
      FSeqInt(isPerfect, eval, s.size)
    }
  }
  override def updateEval(s: (Op, FSeqInt)): (Op, FSeqInt) = {
    val missingTests = state.testsManager.dropFromTests(s._2.totalTests) ++ state.testsManager.newTests.toList
    (s._1, FSeqInt(s._2.correct, s._2.value ++ evalOnTests(s._1, missingTests), s._1.size))
  }
  override def defaultValue(s: Op) = FSeqInt(false, Seq(), s.size)
  override val correct = (e: FSeqInt) => e.correct && e.value.sum == 0
  override val ordering = FSeqIntOrdering
}


class EvalCDGPInt(state: StateCDGP)
                 (implicit opt: Options, coll: Collector)
  extends EvalCDGPDiscrete[FInt](state) {
  override def apply(s: Op, init: Boolean): FInt = {
    if (init) {
      val e = evalOnTests(s, state.testsManager.getTests())
      FInt(false, e, s.size)
    }
    else {
      val (isPerfect, eval) = fitnessCDGPGeneral(s)
      FInt(isPerfect, eval, s.size)
    }
  }
  override def updateEval(s: (Op, FInt)): (Op, FInt) = {
    val missingTests = state.testsManager.dropFromTests(s._2.totalTests) ++ state.testsManager.newTests.toList
    val newFit = FInt(s._2.correct, s._2.value + evalOnTests(s._1, missingTests).sum, s._1.size, state.testsManager.getNumberOfTests)
    (s._1, newFit)
  }
  override def defaultValue(s: Op) = FInt(false, Seq(), s.size)
  override val correct = (e: FInt) => e.correct && e.value == 0
  override val ordering = FIntOrdering
}





abstract class EvalGPRDiscrete[E](state: StateGPR)
                        (implicit opt: Options, coll: Collector)
  extends EvalCDGPDiscrete[E](state) {

  def fitnessGPR: Op => (Boolean, Seq[Int]) = {
    if (state.sygusData.formalInvocations.isEmpty) fitnessOnlyTestCases
    else new Function1[Op, (Boolean, Seq[Int])] {
      def allTestsPassed(evalTests: Seq[Int]): Boolean =
        evalTests.count(_ == 0) == evalTests.size
      def generateAndAddRandomTest(): Unit = {
        if (state.testsManager.newTests.size < maxNewTestsPerIter) {
          val newTest = state.createRandomTest()
          if (newTest.isDefined)
            state.testsManager.addNewTest(newTest.get)
        }
      }
      def apply(s: Op): (Boolean, Seq[Int]) = {
        val evalTests = evalOnTests(s, state.testsManager.getTests())
        if (!doVerify(evalTests))
          (false, evalTests)
        else if (allTestsPassed(evalTests)) {
          // program passes all tests - verify if it is correct
          val (decision, _) = state.verify(s)
          if (decision == "unsat" && evalTests.sum == 0 && (!(state.sygusData.logic == "SLIA") || evalTests.nonEmpty))
            (true, evalTests)  // perfect program found; end of run
          else {
            generateAndAddRandomTest()  // program incorrect; generate random test
            (false, evalTests)
          }
        }
        else {  // program passes enough tests but not all - generate random counterexample
          generateAndAddRandomTest()
          (false, evalTests)
        }
      }
    }
  }

  def fitnessNoVerification(s: Op): (Boolean, Seq[Int]) = {
    (false, evalOnTests(s, state.testsManager.getTests()))
  }
}




class EvalGPRSeqInt(state: StateGPR)
                   (implicit opt: Options, coll: Collector)
  extends EvalGPRDiscrete[FSeqInt](state) {
  override def apply(s: Op, init: Boolean): FSeqInt = {
    if (init) {
      val e = evalOnTests(s, state.testsManager.getTests())
      FSeqInt(false, e, s.size)
    }
    else {
      val (isPerfect, eval) = fitnessGPR(s)
      FSeqInt(isPerfect, eval, s.size)
    }
  }
  override def updateEval(s: (Op, FSeqInt)): (Op, FSeqInt) = {
    val missingTests = state.testsManager.dropFromTests(s._2.totalTests) ++ state.testsManager.newTests.toList
    (s._1, FSeqInt(s._2.correct, s._2.value ++ evalOnTests(s._1, missingTests), s._1.size))
  }
  override def defaultValue(s: Op) = FSeqInt(false, Seq(), s.size)
  override val correct = (e: FSeqInt) => e.correct && e.value.sum == 0
  override val ordering = FSeqIntOrdering
}


class EvalGPRInt(state: StateGPR)
                (implicit opt: Options, coll: Collector)
  extends EvalGPRDiscrete[FInt](state) {
  override def apply(s: Op, init: Boolean): FInt = {
    if (init) {
      val e = evalOnTests(s, state.testsManager.getTests())
      FInt(false, e, s.size)
    }
    else {
      val (isPerfect, eval) = fitnessGPR(s)
      FInt(isPerfect, eval, s.size)
    }
  }
  override def updateEval(s: (Op, FInt)): (Op, FInt) = {
    val missingTests = state.testsManager.dropFromTests(s._2.totalTests) ++ state.testsManager.newTests.toList
    val newFit = FInt(s._2.correct, s._2.value + evalOnTests(s._1, missingTests).sum, s._1.size, state.testsManager.getNumberOfTests)
    (s._1, newFit)
  }
  override def defaultValue(s: Op) = FInt(false, Seq(), s.size)
  override val correct = (e: FInt) => e.correct && e.value == 0
  override val ordering = FIntOrdering
}








//////////////////////////////////////////////////////////////////////////////////////
// EvalFunction - Regression
//////////////////////////////////////////////////////////////////////////////////////



/**
  * Fitness, in which for each program returned is a sequence of absolute differences
  * on the set of test cases.
  */
abstract class EvalCDGPContinuous[E](state: StateCDGP)
                                    (implicit opt: Options, coll: Collector)
  extends EvalCDGP[E, Double](state) {
  // Parameters:
  val optThreshold: Double = getOptThreshold
  coll.set("cdgp.optThreshold", optThreshold)
  val partialConstraintsVisibleForTestsRatio: Boolean = opt('partialConstraintsVisibleForTestsRatio, false)

  checkValidity()


  def getOptThreshold: Double = {
    if (opt.allOptions.contains("optThreshold"))
      opt.paramDouble("optThreshold")
    else {
      val c = opt.paramDouble("optThresholdC", 0.01)
      val allTestsY = state.testsManager.getAllCollectedTests.filter(_._2.isDefined).map(_._2.get.asInstanceOf[Double])
      val s = Tools.stddev(allTestsY)
      (s * c) * (s * c)  // squared because of square in mse
    }
  }

  override def getSize(s: Op): Double = s.size.toDouble

  /**
    * Tests a program on the available tests and returns the vector of 0s (passed test)
    * and 1s (failed test). Depending on the problem will either optimize by executing
    * program directly on the tests, or will have to resort to a solver.
    */
  def evalOnTests(s: Op, tests: Seq[(I, Option[O])]): Seq[Double] = {
    val testsStandard = for (test <- tests) yield { evaluateTest(s, test) }
    if (partialConstraintsInFitness || globalConstraintInFitness || sizeInFitness)
      getConstraintsVector(s, 0.0, 1.0) ++: testsStandard
    else
      testsStandard
  }

  def evaluateTest(s: Op, test: (I, Option[O])): Double = {
    try {
      if (test._2.isDefined)
      // User can define test cases for a problem, in which generally single-answer
      // property does not hold. We will use domain for those cases, since it is more
      // efficient.
        evalTestUsingDomain(s, test)
      else evalTestUsingSolver(s, test)
    }
    catch {
      // return PositiveInfinity for situations like division by 0
      case _: ExceptionIncorrectOperation =>
        Double.PositiveInfinity
      case e: Throwable =>
        handleEvalException(test, s, e.getMessage)
        Double.PositiveInfinity
    }
  }

  /**
    * Checks correctness of the program only for the given test.
    * Tests here always have None as the answer, because in general there is no
    * single answer for the problem being solved in 'solver' mode.
    */
  def evalTestUsingSolver(s: Op, test: (I, Option[O])): Double = {
    val testInput: Map[String, Any] = test._1
    val (dec, _) = state.checkIsProgramCorrectForInput(s, testInput)
    // 0.0 is returned for a correct answer (sat).
    // This becomes questionable if aggregated with error.
    if (dec == "sat") 0.0 else 1.0
  }

  /**
    * Checks correctness of the program only for the given test.
    * If test has a defined expected answer, then it is compared with the answer
    * obtained by executing the program in the domain simulating semantics of SMT
    * theory.
    * If test don't have defined expected answer, then the program's output is verified
    * by the solver for consistency with the specification. The test will be updated if
    * this output is deemed consistent by the solver.
    *
    * Names of variables in test should be the same as those in the function's invocation.
    * They will be renamed for those in the function's declaration.
    */
  def evalTestUsingDomain(s: Op, test: (I, Option[O])): Double = {
    assert(test._2.isDefined, "Trying to domain-evaluate using a test without defined expected output.")
    val testInput: Map[String, Any] = test._1
    val testOutput: Option[Any] = test._2
    val inputVector = state.synthTask.argNames.map(testInput(_))
    val output = domain(s)(inputVector)
    if (output.isEmpty)
      Double.MaxValue   // Recurrence depth was exceeded
    else if (testOutput.isDefined)
      math.abs(output.get.asInstanceOf[Double] - testOutput.get.asInstanceOf[Double])
    else {
      // Situation, when the test case has None as the expected output
      // We don't allow such a situation
      throw new Exception("Trying to domain-evaluate using a test without defined expected output.")

      // The code below can be used to try to find the expected output for the test
      //val (dec, _) = checkIsOutputCorrect(s, testInput, output.get)
      //if (dec == "sat")
      //  testsManager.updateTest((testInput, output))
      //if (dec == "sat") 0 else 1
    }
  }

  def checkValidity(): Unit = {
    var isCorrect = true
    var c: Any = null
    def traverseFun = (t: Term) => {
      t match {
        case LiteralTerm(IntConst(v))    => isCorrect = false; c = v
        case LiteralTerm(StringConst(v)) => isCorrect = false; c = v
        case _ =>
      }
    }
    SygusUtils.traverse(state.sygusData.allConstr, traverseFun)
    if (!isCorrect)
      throw new Exception(s"CDGP for regression requires all number constants in constraints to be of type Real. Problematic constant: $c")
  }

  def isMseCloseToZero(evalTests: Seq[Double]): Boolean = {
    Tools.mse(evalTests) <= optThreshold
  }


  /**
    * Fitness is computed on the test cases. No verification is performed.
    * A solution passing all test cases is considered optimal.
    */
  def fitnessOnlyTestCases: Op => (Boolean, Seq[Double]) =
    (s: Op) => {
      val evalTests = evalOnTests(s, state.testsManager.getTests())
      (isMseCloseToZero(evalTests), evalTests)
    }


  /**
    * Checks, if verification should be conducted. The ration of passed incomplete tests
    * is computed and then compared to testsRatio parameter. If it is higher, then a
    * verification is conducted.
    * @param evalTests Evaluations on all tests.
    */
  def doVerify(evalTests: Seq[Double]): Boolean = {
    // Verify only those solutions which pass all incomplete tests
    lazy val evalConstr = extractTestsConstraints(evalTests)
    var (eval, _) = extractTestsNormal(evalTests).zip(state.testsManager.tests).
      filter { case (_, t) => t._2.isEmpty }.unzip
    eval = if (partialConstraintsVisibleForTestsRatio) evalConstr ++ eval else eval
    val numPassed = eval.count(_ == 0.0).asInstanceOf[Double]
    if (testsDiff.isDefined)
      numPassed >= eval.size - testsDiff.get
    else
      eval.isEmpty || (numPassed / eval.size) >= testsRatio
  }

  /**
    * Creates and adds a new test based on the counterexample returned by solver.
    *
    * @param model Output from the solver for the verification query and sat decision.
    */
  def addNewTest(model: String): Unit = {
    if (state.testsManager.newTests.size < maxNewTestsPerIter) {
      val newTest = state.createTestFromFailedVerification(model)
      if (newTest.isDefined)
        state.testsManager.addNewTest(newTest.get)
    }
  }

  /** Fitness is always computed on the tests that were flushed. */
  def fitnessCDGPRegression: Op => (Boolean, Seq[Double]) =
    if (state.sygusData.formalInvocations.isEmpty) fitnessOnlyTestCases
    else (s: Op) => {
      val evalTests = evalOnTests(s, state.testsManager.getTests())
      // If the program passes the specified ratio of incomplete test cases, it will be
      // verified and a counterexample will be produced (or program will be deemed correct).
      // NOTE: if the program does not pass all test cases, then the probability is high
      // that the produced counterexample will already be in the set of test cases.
      if (!doVerify(evalTests))
        (false, evalTests)
      else {
        val (decision, model) = state.verify(s)
        if (decision == "unsat")
          (isMseCloseToZero(evalTests), evalTests)
        else if (decision == "sat") {
          addNewTest(model.get)
          (false, evalTests)
        }
        else {
          // The 'unknown' or 'timeout' solver's decision. Program potentially may be the optimal
          // solution, but solver is not able to verify this. We proceed by adding no new tests
          // and treating the program as incorrect.
          (false, evalTests)
        }
      }
    }
}





class EvalGPSeqDouble(state: StateCDGP)
                     (implicit opt: Options, coll: Collector, rng: TRandom)
  extends EvalCDGPContinuous[FSeqDouble](state) {
  override def apply(s: Op, init: Boolean): FSeqDouble = {
    val (isPerfect, eval) = fitnessOnlyTestCases(s)
    FSeqDouble(isPerfect, eval, s.size)
  }
  override def updateEval(s: (Op, FSeqDouble)): (Op, FSeqDouble) = {
    val missingTests = state.testsManager.dropFromTests(s._2.totalTests) ++ state.testsManager.newTests.toList
    (s._1, FSeqDouble(s._2.correct, s._2.value ++ evalOnTests(s._1, missingTests), s._1.size))
  }
  override def defaultValue(s: Op) = FSeqDouble(false, Seq(), s.size)
  override val correct = (e: FSeqDouble) => e.correct
  override val ordering = FSeqDoubleOrderingMSE
}


class EvalCDGPSeqDouble(state: StateCDGP)
                       (implicit opt: Options, coll: Collector, rng: TRandom)
  extends EvalGPSeqDouble(state) {
  override def apply(s: Op, init: Boolean): FSeqDouble = {
    if (init) {
      val (_, eval) = fitnessOnlyTestCases(s)
      FSeqDouble(false, eval, s.size) // correctness set to false to not trigger correctness based only on the MSE
    }
    else {
      val (isPerfect, eval) = fitnessCDGPRegression(s)
      FSeqDouble(isPerfect, eval, s.size)
    }
  }
}


class EvalGPDoubleMSE(state: StateCDGP)
                     (implicit opt: Options, coll: Collector, rng: TRandom)
  extends EvalCDGPContinuous[FDouble](state) {
  override def apply(s: Op, init: Boolean): FDouble = {
    val (isPerfect, eval) = fitnessOnlyTestCases(s)
    val mse = Tools.mse(eval)
    FDouble(isPerfect, mse, s.size)
  }
  override def updateEval(s: (Op, FDouble)): (Op, FDouble) = {
    // evalOnTests returns a vector of absolute differences
    val missingTests = state.testsManager.dropFromTests(s._2.totalTests) ++ state.testsManager.newTests.toList
    val newValue = s._2.value + evalOnTests(s._1, missingTests).map{ x => x * x }.sum
    val newFit = FDouble(s._2.correct, newValue, s._1.size)
    (s._1, newFit)
  }
  override def defaultValue(s: Op) = FDouble(false, 0.0, s.size)
  override val correct = (e: FDouble) => e.correct
  override val ordering = FDoubleOrdering
}


class EvalCDGPDoubleMSE(state: StateCDGP)
                       (implicit opt: Options, coll: Collector, rng: TRandom)
  extends EvalGPDoubleMSE(state) {
  override def apply(s: Op, init: Boolean): FDouble = {
    if (init) {
      val (_, eval) = fitnessOnlyTestCases(s)
      val mse = Tools.mse(eval)
      FDouble(false, mse, s.size) // correctness set to false to not trigger correctness based only on the MSE
    }
    else {
      val (isPerfect, eval) = fitnessCDGPRegression(s)
      val mse = Tools.mse(eval)
      FDouble(isPerfect, mse, s.size)
    }
  }
}
package app

import java.io.File

import fuel.core.StatePop
import fuel.func._
import fuel.util.FApp
import swim.eval.LexicaseSelection
import swim.tree._
import sygus.VarDeclCmd
import cdgp._

import scala.collection.Seq
import scala.collection.immutable.Map

/**
  * Helper object that calls the testbed for individual benchmarks
  * WARNING: Does not work fully correct now, probably because of delayed initialization in FApp.
  * This is only for demonstration; the CDGPTestbed object below should be used to run the experiments. 
  */
object TestCDGP extends App {
  val root = System.getProperty("user.dir")
  val collection = "/../SyGuS/resources/sygus14/integer-benchmarks/"
  // val collection = "/../SyGuS/resources/sygus16old/"
  //val collection = "/../ProgramSynthesisBenchmarks/resources/sygus16old/"
  // 0 for one small test, anything else for running on many Sygus benchmarks
  val files = 0 match {
    case 1 => List(new File(root + "/../SyGuS/resources/example.txt"))
    case _ => Tools.getRecursiveListOfFiles(new File(root + collection)).filter(
      !_.getName.matches(".+[0-9][0-9].+"))
    //.filter(f => !f.getParent.contains("multiple-functions"))
  }
  Tools.time {
    for (file <- files) try {
      MainGecco2017.main(Array(
        "--maxGenerations", "20",
        "--printResults", "false",
        "--populationSize", "100",
        "--initMaxTreeDepth", "7",
        "--maxSubtreeDepth", "5",
        "--parEval", "false",
        "--solverPath", "/Users/krawiec/bs/z3/build/z3",
        "--method", "CDGP",
        "--searchAlgorithm", "GP",
        "--benchmark", file.getAbsolutePath
        ))
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}



/**
  * The application object to be used in experiments.
  *
  * Obligatory options:
  * --searchAlgorithm, accepted values: GP, GPSteadyState, Lexicase, LexicaseSteadyState
  * --benchmark, path to the SyGuS benchmark
  * --solverPath, path to the SMT solver (e.g. Z3).
  */
object MainGecco2017 extends FApp {
  assume(opt('parEval, false) == false, "Can't use parallel evaluation because the working collection of tests is not synchronized")

  val searchAlg = opt('searchAlgorithm, "")
  val method = opt('method, "CDGP")
  val MethodCDGP = "CDGP"
  val MethodCDGPconservative = "CDGPcons"
  val MethodGPR = "GPR"
  assume(method == MethodCDGP || method == MethodCDGPconservative || method == MethodGPR)
  assume(searchAlg == "GP" || searchAlg == "GPSteadyState" ||
         searchAlg == "Lexicase" || searchAlg == "LexicaseSteadyState")


  val benchmark = opt('benchmark)
  println(s"Benchmark: $benchmark")

  // Other parameters
  val GPRminInt = opt('GPRminInt, -100)
  val GPRmaxInt = opt('GPRmaxInt, 100)
  val CDGPoneTestPerIter = opt('CDGPoneTestPerIter, false)
  val GPRoneTestPerIter = opt('GPRoneTestPerIter, true)
  val silent = opt('silent, false)
  val sygusOutput = opt('sygusOutput, true)

  val sygusProblem = LoadSygusBenchmark(benchmark)
  //println("SyGuS problem: " + sygusProblem)

  // Retrieve the grammar and signature of the function to be synthesized
  val synthTasks: Seq[SygusSynthesisTask] = ExtractSynthesisTasks(sygusProblem)
  if (synthTasks.size > 1)
    throw new Exception("SKIPPING: Multiple synth-fun commands detected. Cannot handle such problems.")
  val synthTask = synthTasks.head
  val grammar = ExtractSygusGrammar(synthTask)

  val fv = sygusProblem.cmds.collect { case v: VarDeclCmd => v }
  val getValueCommand = f"(get-value (${fv.map(_.sym).mkString(" ")}))"


  // Creating solver manager
  val solverPath = opt('solverPath)
  val solverArgs = opt('solverArgs, "-in")
  val solver = new SolverManager(solverPath, Some(solverArgs), verbose=false)


  // The types for input and output
  type I = Map[String, Any]
  type O = Any
  // testsManager contains all test-related data and encapsulates operations on them.
  val testsManager = new TestsManagerCDGP[I, O]()



  /**
    * Tests a program on the available tests and returns the vector of 0s (passed test) and 1s (failed test)
    * If the desired output is not known for a test, uses a solver to determine it, and adds the obtained
    * desired output to testsWithFoundOutputs
    * 
    */
  def evalOnTests(s: Op, tests: Seq[(I, Option[O])]): Seq[Int] = {
    for (test <- tests) yield {
      val testInputsMap = test._1
      val testOutput = test._2
      // Solver requires proper renaming of variables
      val cexampleRenamed = synthTask.argNames.zip(testInputsMap.unzip._2).toMap
      try {
        val output = LIA(s)(cexampleRenamed)
        // If the desired output for this test is already known, simply compare
        if (testOutput.isDefined) {
          if (output == testOutput.get) 0 else 1
        } else {
          // If the desired output is not known yet, use the solver:
          val checkOnTestCmd = SMTLIBFormatter.checkOnInputAndKnownOutput(sygusProblem, testInputsMap, output,
                                solverTimeout=opt('solverTimeout, 0))
          println("\ncheckOnTestCmd:\n" + checkOnTestCmd)
          val (decision, outputData) = solver.runSolver(checkOnTestCmd)
          // If the program passed the test, we know the desired output and can update
          // the set of tests
          if (decision == "sat") testsManager.updateTest((testInputsMap, Some(output)))
          if (decision == "sat") 0 else 1
        }
      }
      catch {
        case e: Throwable => println(f"Error during evalutation of $s and test $test: ${e.getMessage}"); 1
      }
    }
  }
  
  def tryToFindOutputForTestCase(test: (I, Option[O])): (I, Option[O]) = {
    val cmd: String = SMTLIBFormatter.findOutputForTestCase(sygusProblem, test._1,
                        solverTimeout=opt('solverTimeout, 0))
    //println("\nSearch cmd:\n" + cmd)
    try {
      val getValueCommand = f"(get-value (CorrectOutput))"
      val (decision, res) = solver.runSolver(cmd, getValueCommand)
      // println("Solver res: " + res)
      if (decision == "sat") {
        val values = GetValueParser(res.get)
        (test._1, Some(values.head._2))
      }
      else
        test
    }
    catch {
      case _: Throwable =>
        println(s"Exception during executing query or parsing result, returning test with no output! ")
        test // in case solver returns unknown
    }
  }
  
  def createTestFromFailedVerification(verOutput: String): (Map[String, Any], Option[Any]) = {
    val counterExample = GetValueParser(verOutput) // returns the counterexample
    val testNoOutput = (counterExample.toMap, None) // for this test currently the correct answer is not known
    tryToFindOutputForTestCase(testNoOutput)
  }
  
  def createRandomTest(verOutput: String): (Map[String, Any], Option[Any]) = {
    // The only reason to call the parser here is to get the right argument names:
    val argNames = GetValueParser(verOutput).unzip._1
    val example = argNames.map(argName => (argName, GPRminInt + rng.nextInt(GPRmaxInt+1-GPRminInt)))
    val testNoOutput = (example.toMap, None) // for this test currently the correct answer is not known
    tryToFindOutputForTestCase(testNoOutput)
  }

  def passesAllTests(s: Op): Boolean = {
    val failedTests = evalOnTests(s, testsManager.getTests())
    failedTests.count(_ == 1) > 0
  }

  def verify(s: Op): (String, Option[String]) = {
    val verProblemCmds = SMTLIBFormatter.verifyProblem(sygusProblem, s, solverTimeout=opt('solverTimeout, 0))
    solver.runSolver(verProblemCmds, getValueCommand)
  }

  /**
    * Tests a program on the available tests; if it passes all tests, verifies it and returns the program if
    * it passes verification, wrapped in Left(). Otherwise, returns the vector of 0s and 1s like eval(), only wrapped in Right().
    *
    */
  def fitness(s: Op): Either[Op, Seq[Int]] = {
    val failedTests = evalOnTests(s, testsManager.getTests())
    val numFailed = failedTests.count(_ == 1)
    // CDGP Conservative variant: If the program fails any tests, then don't apply verification to it, 
    // as it is very likely that the found counterexample is already among the tests.
    if (numFailed > 0 && (method == MethodCDGPconservative || method == MethodGPR))
      Right(failedTests)
    else {
      val (decision, r) = verify(s)
      if (decision == "unsat") Left(s) // perfect program found; end of run
      else {
        method match {
          case MethodCDGP | MethodCDGPconservative =>
            if (!CDGPoneTestPerIter ||
                (CDGPoneTestPerIter && testsManager.newTests.isEmpty)) {
              val newTest = createTestFromFailedVerification(r.get)
              testsManager.addNewTest(newTest)
            }
          case MethodGPR => // Generate random example
            if (!GPRoneTestPerIter ||
                (GPRoneTestPerIter && testsManager.newTests.isEmpty)) {
              val newTest = createRandomTest(r.get)
              testsManager.addNewTest(newTest)
            }
        }
        Right(failedTests)
      }
    }
  }

  def updateTestCollections[S, E](s: StatePop[(S, E)]) = {
    val nNew = testsManager.newTests.size
    testsManager.flushHelpers() // resets newTests
    val nKnown = testsManager.tests.values.count(_.isDefined)
    if (!silent)
      println(f"Tests: found: ${nNew}  total: ${testsManager.tests.size}  known outputs: $nKnown")
    s
  }

  def updateEvalInt(s: (Op, Int)): (Op, Int) =
    (s._1, s._2 + evalOnTests(s._1, testsManager.newTests.toList).sum)

  def updateEvalSeqInt(s: (Op, Seq[Int])): (Op, Seq[Int]) =
    (s._1, s._2 ++ evalOnTests(s._1, testsManager.newTests.toList)) // append evals for new tests

  def updatePopSteadyStateGP(update: ((Op, Int))=>(Op, Int) = updateEvalInt)
                            (s: StatePop[(Op, Int)]):
                            StatePop[(Op, Int)] = {
    if (testsManager.newTests.nonEmpty)
      StatePop(s.map{ case (op, e) => update((op, e)) })
    else s
  }
  def updatePopSteadyStateLexicase(update: ((Op, Seq[Int]))=>(Op, Seq[Int]) = updateEvalSeqInt)
                                  (s: StatePop[(Op, Seq[Int])]):
                                  StatePop[(Op, Seq[Int])] = {
    if (testsManager.newTests.nonEmpty)
      StatePop(s.map{ case (op, e) => update(op, e) })
    else s
  }
  def reportStats[E](bsf: BestSoFar[Op, E])(s: StatePop[(Op, E)]) = {
    if (bsf.bestSoFar.isDefined) {
      val (bestOfRun, _) = bsf.bestSoFar.get
      coll.set("result.best.code", bestOfRun)
      coll.set("result.best.smtlib", SMTLIBFormatter.opToString(bestOfRun))
      coll.set("result.best.size", bsf.bestSoFar.get._1.size)
      coll.set("result.best.height", bsf.bestSoFar.get._1.height)
    }
    coll.set("totalTests", testsManager.tests.size)
    coll.set("totalTestsKnownOutputs", testsManager.getNumberOfKnownOutputs)
    coll.set("totalTestsUnknownOutputs", testsManager.getNumberOfUnknownOutputs)
    coll.set("totalSolverCalls", solver.getNumCalls)
    coll.set("totalSolverRestarts", solver.getNumRestarts)
    s
  }
  def epilogueGP(bsf: BestSoFar[Op, Int])(s: StatePop[(Op, Int)]) = {
    // Computes ratio of passed tests for a best of run.
    val (_, e) = bsf.bestSoFar.get
    val totalTests = testsManager.tests.size.toDouble
    val ratio = if (e <= 0) 1.0 else (totalTests - e) / totalTests
    val roundedRatio = BigDecimal(ratio).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.setResult("best.passedTestsRatio", roundedRatio)
    s
  }
  def epilogueLexicase(bsf: BestSoFar[Op, Seq[Int]])(s: StatePop[(Op, Seq[Int])]) = {
    // Computes ratio of passed tests for a best of run.
    val (_, e) = bsf.bestSoFar.get
    val ratio = if (e.head == -1) 1.0 else e.count(_ == 0) / e.size.toDouble
    val roundedRatio = BigDecimal(ratio).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.setResult("best.passedTestsRatio", roundedRatio)
    s
  }
  
  // Construct the search algorithm and run it:
  val (res, bestOfRun) = searchAlg match {

    case "GP" => {
      // Convention: an ideal program has fitness -1.
      def evalGP(s: Op): Int = {
        val r = fitness(s)
        if (r.isLeft) -1 // perfect program found; end of run
        else r.right.get.sum
      }
      def correct = (_: Any, e: Int) => e == -1
      val alg = new SimpleGP[Int](GPMoves(grammar, SimpleGP.defaultFeasible), evalGP, correct) {
        // In our scenario there is no meaning in comparing past best solutions with the current ones,
        // because the current ones have been most likely evaluated on the different set of tests.
        override def epilogue = super.epilogue andThen bsf andThen reportStats(bsf) andThen epilogueGP(bsf)
        override def report = s => s
        override def evaluate = super.evaluate andThen updateTestCollections
      }
      val finalPop = RunExperiment(alg)
      val bestSoFar: Option[(Op, Int)] = alg.bsf.bestSoFar
      (finalPop, bestSoFar)
    }
    
    case "GPSteadyState" => {
      // Convention: an ideal program has fitness -1.
      def evalGP(s: Op): Int = {
        val r = fitness(s)
        if (r.isLeft) -1 // perfect program found; end of run
        else r.right.get.sum
      }
      def correct = (_: Any, e: Int) => e == -1
      val alg = new SimpleSteadyStateEA[Op, Int](GPMoves(grammar, SimpleGP.defaultFeasible), evalGP, correct) {
        override def iter = super.iter andThen updatePopSteadyStateGP() andThen updateTestCollections
        override def epilogue = super.epilogue andThen bsf andThen reportStats(bsf) andThen epilogueGP(bsf)
        override def report = s => s
        override def initialize = super.initialize andThen updatePopSteadyStateGP() andThen updateTestCollections
      }
      val finalPop = RunExperiment(alg)
      val bestSoFar: Option[(Op, Int)] = alg.bsf.bestSoFar
      (finalPop, bestSoFar)
    }

    case "Lexicase" => {
      // Convention: an ideal program has all test outcomes == -1 (but it's enough to check the first one)
      def eval(s: Op): Seq[Int] = {
        val r = fitness(s)
        if (r.isLeft) testsManager.getTests().map(_ => -1) // perfect program found; end of run
        else r.right.get
      }
      def correct = (_: Any, e: Seq[Int]) => e.nonEmpty && e.head == -1
      val alg = new LexicaseGP(GPMoves(grammar, SimpleGP.defaultFeasible), eval, correct) {
        override def iter = super.iter
        override def epilogue = super.epilogue andThen bsf andThen reportStats(bsf) andThen epilogueLexicase(bsf)
        override def report = bsf //s => s
        override def evaluate = super.evaluate andThen updateTestCollections
      }
      val finalPop = RunExperiment(alg)
      (finalPop, alg.bsf.bestSoFar)
    }
    
    case "LexicaseSteadyState" => {
      // Convention: an ideal program has all test outcomes == -1 (but it's enough to check the first one)
      def eval(s: Op): Seq[Int] = {
        val r = fitness(s)
        if (r.isLeft) testsManager.getTests().map(_ => -1) // perfect program found; end of run
        else r.right.get
      }
      def correct = (_: Any, e: Seq[Int]) => e.nonEmpty && e.head == -1
      val selection = new LexicaseSelection[Op, Int](Ordering[Int])
      val deselection = if (opt('lexicaseDeselection, false)) new LexicaseSelection[Op, Int](Ordering[Int].reverse)
                        else new TournamentSelection[Op, Seq[Int]](LongerOrMaxPassedOrdering.reverse)
      implicit val ordering = LongerOrMaxPassedOrdering
      val alg = new SteadyStateEA[Op, Seq[Int]](GPMoves(grammar, SimpleGP.defaultFeasible), eval, correct, selection, deselection) {
        override def iter = super.iter andThen updatePopSteadyStateLexicase() andThen updateTestCollections
        override def epilogue = super.epilogue andThen bsf andThen reportStats(bsf) andThen epilogueLexicase(bsf)
        override def report = s => s
        override def initialize = super.initialize andThen updatePopSteadyStateLexicase() andThen updateTestCollections
      }
      val finalPop = RunExperiment(alg)
      (finalPop, alg.bsf.bestSoFar)
    }

  }

  def isOptimal(bestOfRun: (Op, Any)): Boolean = {
    bestOfRun._2 match {
      case a if a.isInstanceOf[Int]      => a.asInstanceOf[Int] == -1
      case a if a.isInstanceOf[Seq[Int]] => a.asInstanceOf[Seq[Int]].nonEmpty && a.asInstanceOf[Seq[Int]].head == -1
    }
  }

  val passedTestsRatio = coll.getResult("best.passedTestsRatio").getOrElse("n/a")
  println("\nBest program found: " + coll.getResult("best").getOrElse("n/a"))
  println("Evaluation: " + coll.getResult("best.eval").getOrElse("n/a"))
  println("Ratio of passed tests: " + passedTestsRatio)
  println("Total solver calls: " + solver.getNumCalls)
  println("Total time [ms]: " + coll.getResult("totalTimeSystem").getOrElse("Unknown"))
  println("Total tests: " + testsManager.tests.size)


  assume(bestOfRun.isDefined, "No solution (optimal or approximate) to the problem was found.")
  val solutionCode = SMTLIBFormatter.synthTaskSolutionToString(synthTask, bestOfRun.get._1)

  println("\nOPTIMAL SOLUTION:")
  if (isOptimal(bestOfRun.get)) println(solutionCode) else println("unknown")

  if (!isOptimal(bestOfRun.get)) {
    println(f"\nAPPROXIMATED SOLUTION:\n(passedTestsRatio $passedTestsRatio)")
    println(solutionCode)
  }
  solver.close()
}

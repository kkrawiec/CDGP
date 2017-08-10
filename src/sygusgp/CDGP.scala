package sygusgp

import java.io.File

import scala.Left
import scala.Right
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.collection.immutable.Map
import scala.collection.Seq

import fuel.core.StatePop
import fuel.func.RunExperiment
import fuel.util.IApp
import swim.Grammar
import swim.tree.GPMoves
import swim.tree.Op
import swim.tree.SimpleGP
import sygus.VarDeclCmd
import sygus16.SyGuS16
import swim.tree.LexicaseGP
import fuel.util.FApp
import fuel.func.SimpleSteadyStateEA
import swim.eval.LexicaseSelection
import fuel.func.SteadyStateEA
import swim.tree.LongerOrMaxPassedOrdering
import fuel.func.BestSoFar
import fuel.func.TournamentSelection

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
      CDGPTestbed.main(Array(
        "--maxGenerations", "20",
        "--printResults", "false",
        "--populationSize", "100",
        "--initMaxTreeDepth", "7", 
        "--maxSubtreeDepth", "5", 
        "--parEval", "false",
        "--solverPath", "/Users/krawiec/bs/z3/build/z3",
        "--method", "0", 
        "--searchAlgorithm", "GP",
        "--benchmark", file.getAbsolutePath
        ))
    } catch {
      case e: Exception => {
        e.printStackTrace()
      }
    }
  }
}


/**
 * Manages the set of test cases during evolution run.
 */
class TestsManager[I,O]() {
  // Set of counterexamples collected along the run.
  // The Option is None if the desired output for a given input is not known yet.
  val tests = scala.collection.mutable.LinkedHashMap[I, Option[O]]()
  // Set of counterexamples collected from the current generation. To be reseted after each iteration.
  val newTests = scala.collection.mutable.Set[(I, Option[O])]()

  def getTests(): List[(I, Option[O])] = {
    tests.toList
  }
  def getNumberOfKnownOutputs(): Int = tests.values.count(_.isDefined)

  def addNewTest(t: (I, Option[O])) {
    //println("** Trying to add new test: " + t)
    if (!tests.contains(t._1)) {
      //println("** ADDED")
      newTests.+=(t)
    }
  }
  def updateTest(t: (I, Option[O])) {
    //println("** Updated test: " + t)
    tests.put(t._1, t._2)
  }

  /**
   * Moves elements from newTests to a global tests pool, and prepares manager for the next iteration.
   */
  def flushHelpers() {
    // Update the set of tests with the newly found ones:
    for (test <- newTests)
      if (!tests.contains(test._1)) {
        tests.put(test._1, test._2)
      }
    newTests.clear
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
object MainCDGP extends FApp {

  assume(opt('parEval, false) == false, "Can't use parallel evaluation because the working collection of tests is not synchronized")

  val MethodCDGP = 0
  val MethodCDGPconservative = 1
  val MethodGPR = 2
  val method = opt('method, 0)
  assume(method == MethodCDGP || method == MethodCDGPconservative || method == MethodGPR)

  val searchAlg = opt('searchAlgorithm, "")
  assume(searchAlg == "GP" || searchAlg == "GPSteadyState" || searchAlg == "Lexicase" || searchAlg == "LexicaseSteadyState")

  val benchmark = opt('benchmark)
  println(benchmark)
  def loadBenchmark(benchmark: String): Either[String, SyGuS16] = {
    try {
      SyGuS16.parseSyGuS16File(new File(benchmark))
    }
    catch {
      case _: java.io.FileNotFoundException =>
        println(s"File with benchmark not found: $benchmark")
        System.exit(1)
        null
    }
  }
  val parseRes = loadBenchmark(benchmark)
  if (parseRes.isLeft)
    throw new Exception("PARSE ERROR:" + parseRes.left)
  assume(parseRes.isRight)

  // Other parameters
  val GPRminInt = opt('GPRminInt, -100)
  val GPRmaxInt = opt('GPRmaxInt, 100)
  val CDGPoneTestPerIter = opt('CDGPoneTestPerIter, false)
  val GPRoneTestPerIter = opt('GPRoneTestPerIter, true)
  val silent = opt('silent, false)
  
  
  val sygusProblem = parseRes match { case Right(t) => t }
  //println("SyGuS problem: " + sygusProblem)

  // Retrieve the grammar and signature of the function to be synthesized
  val synthTasks = ExtractSynthesisTasks(sygusProblem)
  if (synthTasks.size > 1)
    throw new Exception("SKIPPING: Multiple synth-fun commands detected. Cannot handle such problems.")

  val (fname, grammar, arguments, outputType) = synthTasks.head
  val argNames = arguments.unzip._1

  // Create the Swim grammar from it
  val grammarMap = grammar.toMap
  val start = if (!grammarMap.contains("Start")) grammar.head._1 else "Start"
  val gr = Grammar.fromMap(start, grammarMap)

  val fv = sygusProblem.cmds.collect { case v: VarDeclCmd => v }
  val getValueCommand = f"(get-value (${fv.map(_.sym).mkString(" ")}))"
  


  val solverPath = opt('solverPath)
  var maxSolverRetries = opt('maxSolverRetries, 5)
  var doneSolverRetries = 0
  var solver = createSolver()
  def createSolver(): Solver = {
    // Sometimes during opening connection with a solver unidentified error occurs.
    // This function retries opening connection if this happens.
    coll.set("doneSolverRetries", doneSolverRetries)
    try {
      Solver(solverPath, verbose = false)
    }
    catch {
      case error: Throwable => if (doneSolverRetries < maxSolverRetries) {
          doneSolverRetries += 1
          println(s"Restarting solver ($doneSolverRetries)")
          createSolver()
        }
        else {
          coll.set("solverError", error.getMessage)
          coll.saveSnapshot("error_solver")
          throw error
        }
    }
  }

  /**
    * Executes provided commands using the SMT solver.
    * @param cmd Commands to be executed by the solver.
    * @param postCommands Additional commands to be placed after (check-sat).
    * @return Solver's decision ('sat', 'unsat', 'unknown') and optional content determined by postCommands.
    */
  def runSolver(cmd: String, postCommands: String*): (String, Option[String]) = {
    try {
      solver.solve(cmd, postCommands:_*)
    }
    catch {
      case e : UnknownSolverOutputException => throw e  // we want to fail if any UNKNOWN happens
      case _: Throwable => { // Restarting solver, because most likely it crashed.
        val numCalls = solver.getNumCalls
        solver = createSolver()
        solver.setNumCalls(numCalls) // not losing the current number of solver calls.
        runSolver(cmd, postCommands:_*)
      }
    }
  }
  
  
  
  // The types for input and output
  type I = Map[String, Any]
  type O = Any
  // testsManager contains all test-related data and encapsulates operations on them.
  val testsManager = new TestsManager[I, O]()

  
  
  
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
      val cexampleRenamed = argNames.zip(testInputsMap.unzip._2).toMap
      try {
        val output = LIA(s)(cexampleRenamed)
        // If the desired output for this test is already known, simply compare
        if (testOutput.isDefined) {
          if (output == testOutput.get) 0 else 1
        } else {
          // If the desired output is not known yet, use the solver:
          val checkOnTestCmd = SMTLIBFormatter.checkOnInput(sygusProblem, testInputsMap, output, solverTimeout=opt('solverTimeout, 0))
          println("\ncheckOnTestCmd:\n" + checkOnTestCmd)
          val (decision, outputData) = runSolver(checkOnTestCmd)
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
    val cmd: String = SMTLIBFormatter.searchForCorrectOutput(sygusProblem, test._1, solverTimeout=opt('solverTimeout, 0))
    //println("\nSearch cmd:\n" + cmd)
    try {
      val getValueCommand = f"(get-value (CorrectOutput))"
      val (decision, res) = runSolver(cmd, getValueCommand)
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

  /**
    * Tests a program on the available tests; if it passes all tests, verifies it and returns the program if
    * it passes verification, wrapped in Left(). Otherwise, returns the vector of 0s and 1s like eval(), only wrapped in Right().
    *
    */
  def fitness(s: Op): Either[Op, Seq[Int]] = {
    val failedTests = evalOnTests(s, testsManager.getTests())
    val cntFailed = failedTests.count(_ == 1)
    // CDGP Conservative variant: If the program fails any tests, then don't apply verification to it, 
    // as it is very likely that the found counterexample is already among the tests.
    if (cntFailed > 0 && (method == MethodCDGPconservative || method == MethodGPR))
      Right(failedTests)
    else {
      val verificationProblem = SMTLIBFormatter.verify(sygusProblem, s, solverTimeout=opt('solverTimeout, 0))
      val (decision, r) = runSolver(verificationProblem, getValueCommand) //solver.solve(verificationProblem, getValueCommand)
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

  def updateTestCollections[E](s: StatePop[(Op, E)]) = {
    val nNew = testsManager.newTests.size
    testsManager.flushHelpers() // resets newTests
    val nKnown = testsManager.tests.values.count(_.isDefined)
    if (!silent)
      println(f"Tests: found: ${nNew}  total: ${testsManager.tests.size}  known outputs: $nKnown")
    s
  }
  def updatePopSteadyStateGP(s: StatePop[(Op, Int)]): StatePop[(Op, Int)] = {
    if (testsManager.newTests.size > 0)
      StatePop(s.map{ case (op, e) =>
        val x = (op, e + evalOnTests(op, testsManager.newTests.toList).sum)
        x })
    else
      s
  }
  def updatePopSteadyStateLexicase(s: StatePop[(Op, Seq[Int])]): StatePop[(Op, Seq[Int])] = {
    if (testsManager.newTests.size > 0)
      StatePop(s.map{ case (op, e) =>
        val x = (op, e ++ evalOnTests(op, testsManager.newTests.toList)) // append evals for new tests
        x })
    else
      s
  }
  def reportStats[E](bsf: BestSoFar[Op, E])(s: StatePop[(Op, E)]) = {
    if (bsf.bestSoFar.isDefined) {
      coll.set("result.best.size", bsf.bestSoFar.get._1.size)
      coll.set("result.best.height", bsf.bestSoFar.get._1.height)
    }
    coll.set("totalTests", testsManager.tests.size)
    coll.set("totalKnownTestsOutputs", testsManager.getNumberOfKnownOutputs())
    coll.set("totalSolverCalls", solver.getNumCalls)
    s
  }
  def epilogueGP(bsf: BestSoFar[Op, Int])(s: StatePop[(Op, Int)]) = {
    // Computes ratio of passed tests for a best of run.
    val (op, e) = bsf.bestSoFar.get
    val totalTests = testsManager.tests.size.toDouble
    val ratio = if (e <= 0) 1.0 else (totalTests - e).toDouble / totalTests
    coll.setResult("best.passedTestsRatio", ratio)
    s
  }
  def epilogueLexicase(bsf: BestSoFar[Op, Seq[Int]])(s: StatePop[(Op, Seq[Int])]) = {
    // Computes ratio of passed tests for a best of run.
    val (op, e) = bsf.bestSoFar.get
    val ratio = if (e.head == -1) 1.0 else e.count(_ == 0) / e.size.toDouble
    coll.setResult("best.passedTestsRatio", ratio)
    s
  }
  def printPop[E](s: StatePop[(Op, E)]) = {
    println("\nPopulation:")
    for (x <- s)
      println(x)
    println()
    s
  }
  
  // Construct the search algorithm and run it:
  val res = searchAlg match {

    case "GP" => {
      // Convention: an ideal program has fitness -1.
      def evalGP(s: Op) = {
        val r = fitness(s)
        if (r.isLeft) -1 // perfect program found; end of run
        else r.right.get.sum
      }
      def correct = (_: Any, e: Int) => e == -1
      val alg = new SimpleGP(GPMoves(gr, SimpleGP.defaultFeasible), evalGP, correct) {
        // In our scenario there is no meaning in comparing past best solutions with the current ones,
        // because the current ones have been most likely evaluated on the different set of tests.
        override def epilogue = super.epilogue andThen bsf andThen reportStats(bsf) andThen epilogueGP(bsf)
        override def report = s => s
        override def evaluate = super.evaluate andThen updateTestCollections
      }
      RunExperiment(alg)
    }
    
    case "GPSteadyState" => {
      // Convention: an ideal program has fitness -1.
      def evalGP(s: Op) = {
        val r = fitness(s)
        if (r.isLeft) -1 // perfect program found; end of run
        else r.right.get.sum
      }
      def correct = (_: Any, e: Int) => e == -1
      val alg = new SimpleSteadyStateEA(GPMoves(gr, SimpleGP.defaultFeasible), evalGP, correct) {
        override def iter = super.iter andThen updatePopSteadyStateGP andThen updateTestCollections
        override def epilogue = super.epilogue andThen bsf andThen reportStats(bsf) andThen epilogueGP(bsf)
        override def report = s => s
        override def initialize = super.initialize andThen updatePopSteadyStateGP andThen updateTestCollections
      }
      RunExperiment(alg)
    }

    case "Lexicase" => {
      // Convention: an ideal program has all test outcomes == -1 (but it's enough to check the first one)
      def eval(s: Op) = {
        val r = fitness(s)
        if (r.isLeft) 0.until(testsManager.tests.size).map(_ => -1) // perfect program found; end of run
        else r.right.get
      }
      def correct = (_: Any, e: Seq[Int]) => e.nonEmpty && e.head == -1
      val alg = new LexicaseGP(GPMoves(gr, SimpleGP.defaultFeasible), eval, correct) {
        override def iter = super.iter //andThen updateTestCollections
        override def epilogue = super.epilogue andThen bsf andThen reportStats(bsf) andThen epilogueLexicase(bsf)
        override def report = s => s
        override def evaluate = super.evaluate andThen updateTestCollections
      }
      RunExperiment(alg)
    }
    
    case "LexicaseSteadyState" => {
      // Convention: an ideal program has all test outcomes == -1 (but it's enough to check the first one)
      def eval(s: Op): Seq[Int] = {
        val r = fitness(s)
        if (r.isLeft) 0.until(testsManager.tests.size).map(_ => -1) // perfect program found; end of run
        else r.right.get
      }
      def correct = (_: Any, e: Seq[Int]) => e.nonEmpty && e.head == -1
      val selection = new LexicaseSelection[Op, Int](Ordering[Int])
      val deselection = if (opt('lexicaseDeselection, false)) new LexicaseSelection[Op, Int](Ordering[Int].reverse)
                        else new TournamentSelection[Op, Seq[Int]](LongerOrMaxPassedOrdering.reverse)
      implicit val ordering = LongerOrMaxPassedOrdering
      val alg = new SteadyStateEA[Op, Seq[Int]](GPMoves(gr, SimpleGP.defaultFeasible), eval, correct, selection, deselection) {
        override def iter = super.iter andThen updatePopSteadyStateLexicase andThen updateTestCollections
        override def epilogue = super.epilogue andThen bsf andThen reportStats(bsf) andThen epilogueLexicase(bsf)
        override def report = s => s
        override def initialize = super.initialize andThen updatePopSteadyStateLexicase andThen updateTestCollections
      }
      RunExperiment(alg)
    }

  }


  
  println("\nBest program found: " + coll.getResult("best").getOrElse("None"))
  println("Evaluation: " + coll.getResult("best.eval").getOrElse("None"))
  println("Ratio of passed tests: " + coll.getResult("best.passedTestsRatio").getOrElse("None"))
  println("Total solver calls: " + solver.getNumCalls)
  println("Total time [ms]: " + coll.getResult("totalTimeSystem").getOrElse("Unknown"))
  println("Total tests: " + testsManager.tests.size)
  solver.close
}


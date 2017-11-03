package tests

import java.io.File

import cdgp._
import sygus.{BoolSortExpr, IntSortExpr, SortExpr, VarDeclCmd}
import scala.util.Random


object SolverCorrectOutput extends App {
  private val rng: Random = scala.util.Random
  rng.setSeed(0)
  val root = System.getProperty("user.dir")
  println(s"Working directory: $root")

  val collection = "/resources/LIA/cdgp_paper17/"
  // val files = Tools.getRecursiveListOfFiles(new File(root + collection)).filter{ f => f.getName.endsWith(".sl")}
  val files = List(new File(root + "/resources/LIA/Median3_tests.sl"))
  val solverPath = Global.solverPath
  val solver = SolverInteractive(solverPath, verbose = false)


  def getRandomInput(vars: Seq[(String, SortExpr)]): Seq[(String, AnyVal)] =
    vars.map {
      case (name, IntSortExpr())  => name -> (rng.nextInt(61) - 30)
      case (name, BoolSortExpr()) => name -> rng.nextBoolean
    }

  for (file <- files) {
    println("-" * 100)
    println(s"File: ${file.getAbsolutePath}")
    println("-" * 100)

    val sygusProblem = LoadSygusBenchmark(file)
    val synthTask = SygusSynthesisTask(sygusProblem).head
    val sygusConstr = SygusBenchmarkConstraints(sygusProblem, synthTask)
    val varDeclsMap = sygusConstr.varDecls.map { v: VarDeclCmd => (v.sym, v) }.toMap
    val inv: Seq[String] = sygusConstr.formalInvocations.head
    println("varDeclsMap: " + varDeclsMap)
    val inputsSortsMap = inv.map{ x => (x, varDeclsMap(x).sortExpr) }
    val getValueCommand = s"(get-value (${inv.mkString(" ")} CorrectOutput))"

    // Generate 100 random inputs and find correct outputs for them
    // TODO: fix QueryTemplateFindOutput
    val templateFindOutput = new QueryTemplateFindOutput(sygusProblem, sygusConstr)
    0.until(100).foreach { _ =>
      val input = getRandomInput(inputsSortsMap).toMap
      val query = templateFindOutput(input)
      println("query: " + query)
      val (dec, res) = solver.solve(query, getValueCommand)
      println(s"Input: $input")
      println(s"Decision: $dec, model: $res")
    }
  }
}

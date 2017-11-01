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

  val collection = "/resources/LIA/other"
  val files = Tools.getRecursiveListOfFiles(new File(root + collection)).filter{ f =>
    f.getName.endsWith(".sl")}
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
    val varDeclsMap = sygusProblem.cmds.collect { case v: VarDeclCmd => (v.sym, v) }.toMap
    val inv: Seq[String] = SygusUtils.getSynthFunsInvocationsInfo(sygusProblem,
      synthTask.fname).head
    val inputsSortsMap = inv.map{ x => (x, varDeclsMap(x).sortExpr) }
    val getValueCommand = s"(get-value (${inv.mkString(" ")} CorrectOutput))"

    // Generate 100 random inputs and find correct outputs for them
    val templateFindOutput = new QueryTemplateFindOutput(sygusProblem, sygusConstr)
    0.until(100).foreach { _ =>
      val input = getRandomInput(inputsSortsMap).toMap
      val query = templateFindOutput(input)
      val (dec, res) = solver.solve(query, getValueCommand)
      println(s"Decision: $dec, model: $res")
    }
  }
}

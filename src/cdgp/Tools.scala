package cdgp

import java.io.File

import fuel.util.{Options, OptionsMap}



object Tools {
  // From Jerry's library:
  def getRecursiveListOfFiles(dir: File): List[File] = {
    val these = dir.listFiles.toList
    these.filter(!_.isDirectory) ++
      these.filter(_.isDirectory).flatMap(getRecursiveListOfFiles)
  }

  // Pretty printer for arbitrarily nested case classes
  def pretty(p: Any, depth: Int = 0): String =
    " " * depth + (p match {
      case prod: Product => prod.getClass.getSimpleName + "(\n" + prod.productIterator
        .map(a => pretty(a, depth + 1)).fold("")(_ + _) + " " * depth + ")"
      case _ => p.toString
    }) + "\n"

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
    result
  }

  def stddev(xs: List[Double], avg: Double): Double = xs match {
    case Nil => -1.0
    case ys => math.sqrt((0.0 /: ys) {
      (a,e) => a + math.pow(e - avg, 2.0)
    } / xs.size)
  }

}
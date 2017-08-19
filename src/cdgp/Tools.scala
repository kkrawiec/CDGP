package cdgp

import java.io.File


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

}
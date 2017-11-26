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

  def allOccurences(s: String, x: String): List[Int] = {
    var list = List[Int]()
    var i = 0
    do {
      val ind = s.indexOf(x, i)
      if (ind == -1)
        i = s.size
      else {
        list = ind :: list
        i = ind + 1
      }
    } while (i < s.size)
    list.reverse
  }

  /**
    * Finds in the string all hex encoded chars (e.g. \x00) and converts them to chars.
    * Additionally replaces \n, \t and other such sequences for their appropriate chars.
    */
  def convertToJavaString(s: String): String = {
    var res = ""
    var i = 0
    while (i < s.size) {
      if (i <= s.size-4 && s.charAt(i) == '\\' && s.charAt(i+1) == 'x') {
        val d1 = hexToInt(s.charAt(i+2))
        val d2 = hexToInt(s.charAt(i+3))
        res += (d1*16 + d2).toChar
        i += 4
      }
      else if (i <= s.size-2 && s.charAt(i) == '\\') {
        /* Z3 handles following special chars:
          \a 	audible bell 	byte 0x07 in ASCII encoding
          \b 	backspace 	byte 0x08 in ASCII encoding
          \f 	form feed - new page 	byte 0x0c in ASCII encoding
          \n 	line feed - new line 	byte 0x0a in ASCII encoding
          \r 	carriage return 	byte 0x0d in ASCII encoding
          \t 	horizontal tab 	byte 0x09 in ASCII encoding
          \v 	vertical tab 	byte 0x0b in ASCII encoding
          Source: https://rise4fun.com/z3/tutorialcontent/sequences
        */
        val c = s.charAt(i+1) match {
          case 'a' => 7.toChar.toString
          case 'b' => "\b"
          case 'f' => "\f"
          case 'n' => "\n"
          case 'r' => "\r"
          case 't' => "\t"
          case 'v' => 9.toChar.toString
          case _   => ""
        }
        if (c != "") {
          res += c
          i += 2
        }
        else {
          res += s.charAt(i)
          i += 1
        }
      }
      else if (i <= s.size-2 && s.charAt(i) == '\"' && s.charAt(i+1) == '\"') {
        res += "\""
        i += 2
      }
      else {
        res += s.charAt(i)
        i += 1
      }
    }
    res
  }

  def hexToInt(c: Char): Int = {
    if (c <= '9') c.asDigit
    else c match {
      case 'a' | 'A' => 10
      case 'b' | 'B' => 11
      case 'c' | 'C' => 12
      case 'd' | 'D' => 13
      case 'e' | 'E' => 14
      case 'f' | 'F' => 15
    }
  }
}
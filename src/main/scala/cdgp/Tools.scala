package cdgp

import java.io.File
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale

import fuel.util.Options

import scala.util.Random



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

  val df = new DecimalFormat("0.0", DecimalFormatSymbols.getInstance(Locale.ENGLISH))
  df.setMaximumFractionDigits(340)
  /**
    * Convert Double to String using dot punctuation and without scientific notation.
    */
  def double2str(d: Double): String = df.format(d)
  def double2str(d: Seq[Double]): Seq[String] = d.map(double2str(_))

  def stringScientificNotation(seq: Seq[Double], numDigits: Int = 2): String = {
    import java.text.DecimalFormat
    val formatter = new DecimalFormat(s"0.${"#" * numDigits}E0", DecimalFormatSymbols.getInstance(Locale.ENGLISH))
    seq.map(formatter.format(_)).mkString(",")
  }

  def avg(xs: Seq[Double]): Double = {
    if (xs.isEmpty) throw new Exception("Trying to compute average from an empty list!")
    else xs.sum / xs.size
  }

  def stddev(xs: Seq[Double]): Double = stddev(xs, avg(xs))
  def stddev(xs: Seq[Double], avg: Double): Double = xs match {
    case Nil => -1.0
    case ys => math.sqrt((0.0 /: ys) {
      (a,e) => a + math.pow(e - avg, 2.0)
    } / xs.size)
  }

  /** Computes MSE of the list of errors. */
  def mse(xs: Seq[Double]): Double =
    if (xs.isEmpty) 0.0 else {
      try {
        val mse = xs.map(x => x * x).sum / xs.size
        if (mse >= 0.0) mse
        else Double.MaxValue  // MSE cannot be negative, return max double
      }
      catch {
        case _: Throwable => Double.MaxValue  // in case there is some error with overflow
      }
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
  def convertSmtToJavaString(s: String): String = {
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
          case 'a' => Character.toString(7)
          case 'b' => "\b"
          case 'f' => "\f"
          case 'n' => "\n"
          case 'r' => "\r"
          case 't' => "\t"
          case 'v' => Character.toString(11)
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

  /** Duplicates each element in the sequence n times. **/
  def duplicateElements[T](seq: Seq[T], n: Int): Seq[T] = {
    if (n == 1) seq else seq.flatMap(Seq.fill(n)(_))
  }


  /**
   * Given a command line parameter, it checks if it ends with '%' or not. If yes,
   * then the number of tests is computed as a ratio of the totalTests. Otherwise,
   * the specified fixed number of tests is returned.
   */
  def getNumTests(optText: Option[String], totalTests: Int): Option[Int] = {
    if (optText.isEmpty) None
    else {
      val text = optText.get
      if (text.last.equals('%')) {
        val prefix = text.take(text.size - 1)
        Some((prefix.toDouble * totalTests / 100.0).toInt)
      }
      else Some(text.toInt)
    }
  }

  /**
   * Divides a set of test cases on a train set, validation set, and test set (in this order in
   * the returned tuple).
   */
  def splitTrainValidationTest[A](tests: Seq[A])(implicit opt: Options): (Seq[A], Seq[A], Seq[A]) = {
    val optTrain: Option[Int] = getNumTests(opt.getOptionString("sizeTrainSet"), tests.size)
    val nValid: Int = getNumTests(Some(opt.getOption("sizeValidationSet", "0")), tests.size).get
    val nTest: Int = getNumTests(Some(opt.getOption("sizeTestSet", "0")), tests.size).get
    val nTrain = optTrain.getOrElse(tests.size - nValid - nTest)
    val (tTrain, tValid, tTest) = splitTrainValidationTest(tests, nTrain, nValid, nTest, opt('shuffleData, true))
    (tTrain, tValid, tTest)
  }

  /**
   * Divides a set of test cases on a train set and test set.
   */
  def splitTrainValidationTest[A](tests: Seq[A], nTrain: Int, nValid: Int, nTest: Int, shuffle: Boolean = true): (Seq[A], Seq[A], Seq[A]) = {
    assert(nTrain >= 0, "Number of training examples must be nonnegative.")
    assert(nValid >= 0, "Number of validation examples must be nonnegative.")
    assert(nTest >= 0, "Number of test examples must be nonnegative.")
    assert(nTrain+nValid+nTest <= tests.size, "There are not enough tests to create the specified training, validation, and test sets.")
    val shuffled = if (shuffle) Random.shuffle(tests) else tests
    (shuffled.take(nTrain), shuffled.slice(nTrain, nTrain + nValid), shuffled.slice(nTrain + nValid, nTrain + nValid + nTest))
  }
}
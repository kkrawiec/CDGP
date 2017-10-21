package cdgp

import swim.RecursiveDomain
import scala.collection.Seq


class LIA(val funArgsNames: Seq[String], funName: String, recDepth: Int = 100)
  extends RecursiveDomain[Any, Any](funArgsNames.size, recDepth, recSymbol = funName, iteSymbol = "ite") {

  def divide(m: Int, n: Int, smtlibSem: Boolean = true): Int = {
    if (n == 0) throw new Exception("Division by 0 during evaluation!")
    if (smtlibSem) {
      /* "Regardless of sign of m,
          if n is positive, (div m n) is the floor of the rational number m/n;
          if n is negative, (div m n) is the ceiling of m/n."
        Source: http://smtlib.cs.uiowa.edu/theories-Ints.shtml
      */
      val quotient = m.asInstanceOf[Double] / n
      if (n > 0) math.floor(quotient).toInt
      else math.ceil(quotient).toInt
    }
    else m / n  // standard, common for programming languages division (always rounding in the direction of 0).
  }

  def modulo(m: Int, n: Int, smtlibSem: Boolean = true): Int = {
    if (n == 0) throw new Exception("Modulo by 0 during evaluation!")
    if (smtlibSem)
      /*In SMTLIB m % n is equal: m - (m div n) * n and is always non-negative.
       * Example:
       *  5 %  3 =  2
       *  5 % -3 =  2
       * -5 %  3 = 1
       * -5 % -3 = 1
       */
       m - divide(m, n) * n
    else
      /*In Scala m % n has the following semantic:
      * if m is positive, (mod m n) is >= 0 and is equivalent to (mod (abs m) (abs n))
      * if m is negative, (mod m n) is <= 0 and is equivalent to -(mod (abs m) (abs n))
      * Example:
      *  5 %  3 =  2
      *  5 % -3 =  2
      * -5 %  3 = -2
      * -5 % -3 = -2
      */
      m % n
  }

  override def operationalSemantics(input: Seq[Any])(childRes: Seq[Any]): Any = {
    childRes match {
      case Seq("+", x: Int, y: Int)                => x + y
      case Seq("-", x: Int)                        => -x
      case Seq("-", x: Int, y: Int)                => x - y
      case Seq("*", x: Int, y: Int)                => x * y  // in LIA x or y must be a constant
      case Seq("div", x: Int, y: Int)              => divide(x, y)
      case Seq("mod", x: Int, y: Int)              => modulo(x, y)
      case Seq("abs", x: Int)                      => if (x >= 0) x else -x
      case Seq("<", x: Int, y: Int)                => x < y
      case Seq("<=", x: Int, y: Int)               => x <= y
      case Seq(">", x: Int, y: Int)                => x > y
      case Seq(">=", x: Int, y: Int)               => x >= y
      // ite is handled specially by the RecursiveDomain
      // case Seq('ite, b: Boolean, x: Int, y: Int) => if (b) x else y
      case Seq("and", a: Boolean, b: Boolean)      => a && b
      case Seq("or", a: Boolean, b: Boolean)       => a || b
      case Seq("xor", a: Boolean, b: Boolean)      => a ^ b
      case Seq("=>", a: Boolean, b: Boolean)       => !a || b
      case Seq("not", b: Boolean)                  => !b
      case Seq(s: String) if funArgsNames.contains(s) =>
        val i = funArgsNames.indexOf(s)
        if (i == -1) throw new Exception("Unrecognized variable name!")
        input(i)
      case Seq(s: String)                          => s // String constant
      case Seq(v: Int)                             => v
      case Seq(v: Boolean)                         => v
      case Seq("=", x: Any, y: Any)                => x == y
      case Seq("distinct", a: Any, b: Any)         => a != b
      case Seq(_: Symbol, xs@_*)                   =>
        throw new Exception("In evaluation Symbols for op names are not supported. Use Strings instead. ")
      case instr @ _                             => throw new Exception("Invalid instruction: " + instr)
    }
  }
}


object LIA {
  def apply(funArgsNames: Seq[String], funName: String, recDepth: Int = 100) =
    new LIA(funArgsNames, funName, recDepth)
}

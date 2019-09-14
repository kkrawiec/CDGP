package cdgp

import java.util.regex.{Matcher, Pattern}

import swim.RecursiveDomain

import scala.collection.Seq


final case class ExceptionIncorrectOperation(message: String,
                                             cause: Throwable = None.orNull)
  extends Exception(message, cause)



class DomainSLIA(val funArgsNames: Seq[String], funName: Symbol, recDepth: Int = 100)
  extends RecursiveDomain[Any, Any](funArgsNames.size, recDepth, recSymbol = funName, iteSymbol = 'ite) {

  def divide(m: Int, n: Int, smtlibSem: Boolean = true): Int = {
    if (n == 0) throw ExceptionIncorrectOperation("Division by 0 during evaluation!")
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
    if (n == 0) throw ExceptionIncorrectOperation("Modulo by 0 during evaluation!")
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
    val input2 = input.map{ x: Any =>
      if (!x.isInstanceOf[String]) x
      else
        // Convert hex encoding of chars (e.g. \x00) sometimes returned by solver.
        Tools.convertSmtToJavaString(x.asInstanceOf[String])
    }
    operationalSemanticsInternal(input2)(childRes)
  }

  private def operationalSemanticsInternal(input: Seq[Any])(childRes: Seq[Any]): Any = {
    childRes match {
      // Arithmetic
      case Seq('+, x: Int, y: Int)                => x + y
      case Seq('-, x: Int)                        => -x
      case Seq('-, x: Int, y: Int)                => x - y
      case Seq('*, x: Int, y: Int)                => x * y  // in LIA x or y must be a constant
      case Seq('div, x: Int, y: Int)              => divide(x, y)
      case Seq('mod, x: Int, y: Int)              => modulo(x, y)
      case Seq('abs, x: Int)                      => if (x >= 0) x else -x
      case Seq('<, x: Int, y: Int)                => x < y
      case Seq('<=, x: Int, y: Int)               => x <= y
      case Seq('>, x: Int, y: Int)                => x > y
      case Seq('>=, x: Int, y: Int)               => x >= y

      // ite is handled specially by the RecursiveDomain
      // case Seq('ite, b: Boolean, x: Int, y: Int) => if (b) x else y
      case Seq('and, a: Boolean, b: Boolean)      => a && b
      case Seq('or, a: Boolean, b: Boolean)       => a || b
      case Seq('xor, a: Boolean, b: Boolean)      => a ^ b
      case Seq('=>, a: Boolean, b: Boolean)       => !a || b
      case Seq('not, b: Boolean)                  => !b

      // Universal operations
      case Seq('=, x: Any, y: Any)                => x == y
      case Seq('distinct, x: Any, y: Any)         => x != y

      // Variables and constants
      case Seq(s: Symbol) if funArgsNames.contains(s.name) =>
        val i = funArgsNames.indexOf(s.name)
        if (i == -1) throw new Exception("Unrecognized variable name!")
        input(i)
      case Seq(s: String)                          => s // String constant
      case Seq(v: Int)                             => v
      case Seq(v: Boolean)                         => v

      // Strings
      case Seq(Symbol("str.len"), s: String) => s.size
      case Seq(Symbol("str.++"), s1: String, s2: String) => s1 + s2
      case Seq(Symbol("str.at"), s: String, i: Int) => if (i >= 0 && i < s.size) s.charAt(i).toString else ""
      case Seq(Symbol("str.replace"), s: String, a: String, b: String) =>
        if (a == "") s else s.replaceFirst(Pattern.quote(a), Matcher.quoteReplacement(b))
      case Seq(Symbol("str.substr"), s: String, a: Int, b: Int) =>
        if (a < 0 || b <= 0 || a >= s.size) ""
        else if (a+b >= s.size) s.substring(a, s.size)
        else s.substring(a, a + b)
      case Seq(Symbol("str.indexof"), s: String, k: String, i: Int) =>
        if (i < 0 || i > s.size) -1  // indexOf in Scala is less strict about index
        else s.indexOf(k, i)
      case Seq(Symbol("str.prefixof"), s: String, k: String) => k.startsWith(s)
      case Seq(Symbol("str.suffixof"), s: String, k: String) => k.endsWith(s)
      case Seq(Symbol("str.contains"), s: String, k: String) => s.contains(k)
      case Seq(Symbol("int.to.str"), i: Int) => if (i < 0) "" else i.toString
      case Seq(Symbol("str.to.int"), s: String) =>
        try {
          val x = s.toInt
          if (x < 0) -1 else x
        } catch {case _: Throwable => -1}

      // Exceptions
      case Seq(_: String, xs@_*) =>
        throw new Exception("In evaluation Strings for op names are not supported. Use Symbols instead. ")
      case instr @ _ =>
        throw new Exception("Invalid instruction: " + instr)
    }
  }
}


object DomainSLIA {
  def apply(funArgsNames: Seq[String], funName: Symbol, recDepth: Int = 100) =
    new DomainSLIA(funArgsNames, funName, recDepth)
}



////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////



class DomainReals(val funArgsNames: Seq[String], funName: Symbol, recDepth: Int = 100)
  extends RecursiveDomain[Any, Any](funArgsNames.size, recDepth, recSymbol = funName, iteSymbol = 'ite) {

  def divide(x: Double, y: Double): Double = {
    if (y == 0.0) throw ExceptionIncorrectOperation("Division by 0 during evaluation!")
    else x / y
  }

  def operationalSemantics(input: Seq[Any])(childRes: Seq[Any]): Any = {
    childRes match {
      // Variables and constants
      case Seq(s: Symbol) if funArgsNames.contains(s.name) =>
        val i = funArgsNames.indexOf(s.name)
        if (i == -1) throw new Exception("Unrecognized variable name!")
        input(i)
      case Seq(v: Double)                          => v
      case Seq(v: Boolean)                         => v

      // Arithmetic
      case Seq('+, x: Double, y: Double)           => x + y
      case Seq('-, x: Double)                      => -x
      case Seq('-, x: Double, y: Double)           => x - y
      case Seq('*, x: Double, y: Double)           => x * y  // in LIA x or y must be a constant
      case Seq(Symbol("/"), x: Double, y: Double)  => divide(x, y)
      case Seq('abs, x: Double)                    => if (x >= 0.0) x else -x
      case Seq('<, x: Double, y: Double)           => x < y
      case Seq('<=, x: Double, y: Double)          => x <= y
      case Seq('>, x: Double, y: Double)           => x > y
      case Seq('>=, x: Double, y: Double)          => x >= y

      // ite is handled specially by the RecursiveDomain
      // case Seq('ite, b: Boolean, x: Int, y: Int) => if (b) x else y
      case Seq('and, a: Boolean, b: Boolean)      => a && b
      case Seq('or, a: Boolean, b: Boolean)       => a || b
      case Seq('xor, a: Boolean, b: Boolean)      => a ^ b
      case Seq('=>, a: Boolean, b: Boolean)       => !a || b
      case Seq('not, b: Boolean)                  => !b

      // Universal operations
      case Seq('=, x: Any, y: Any)                => x == y
      case Seq('distinct, x: Any, y: Any)         => x != y

      // Transcendental functions
      case Seq('sqrt, x: Double) => math.sqrt(x)
      case Seq('exp, x: Double) => math.exp(x)
      case Seq('log | 'ln, x: Double) => math.log(x)  // natural logarithm
      case Seq('^ | 'pow, x: Double, y: Double) => math.pow(x, y)
      case Seq('sin, x: Double) => math.sin(x)  // x in radians
      case Seq('cos, x: Double) => math.cos(x)  // x in radians
      case Seq('tan, x: Double) => math.tan(x)  // x in radians
      case Seq('asin | 'arcsin, x: Double) => math.asin(x)  // x in radians
      case Seq('acos | 'arccos, x: Double) => math.acos(x)  // x in radians
      case Seq('atan | 'arctan, x: Double) => math.atan(x)  // x in radians
      case Seq('sinh, x: Double) => math.sinh(x)  // x in radians
      case Seq('cosh, x: Double) => math.cosh(x)  // x in radians
      case Seq('tanh, x: Double) => math.tanh(x)  // x in radians

      // Exceptions
      case Seq(_: String, xs@_*) =>
        throw new Exception("In evaluation Strings for op names are not supported. Use Symbols instead.")
      case instr @ _ =>
        throw new Exception("Invalid instruction: " + instr)
    }
  }
}


object DomainReals {
  def apply(funArgsNames: Seq[String], funName: Symbol, recDepth: Int = 100) =
    new DomainReals(funArgsNames, funName, recDepth)
}

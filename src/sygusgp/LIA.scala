package sygusgp

import swim.Domain
import swim.tree.Op
import scala.collection.immutable.Map
import scala.collection.immutable.Seq


case object LIA extends Domain[Map[String, Any], Any, Op] {
  override def semantics(input: Map[String, Any]) = {
    new Function1[Op, Any] {
      def apply(op: Op): Any = {
        // Needs toList (otherwise ArrayBuffer, which doesn't work with patter matching)
        val childRes = op.args.toSeq.map(c => apply(c)).toList
        childRes.+:(op.op) match {
          case Seq("+", x: Int, y: Int)               => x + y
          case Seq("-", x: Int, y: Int)               => x - y
          case Seq("=", x: Int, y: Int)               => x == y
          case Seq("<", x: Int, y: Int)               => x < y
          case Seq("<=", x: Int, y: Int)              => x <= y
          case Seq(">", x: Int, y: Int)               => x > y
          case Seq(">=", x: Int, y: Int)              => x >= y
          case Seq("ite", b: Boolean, x: Int, y: Int) => if (b) x else y
          case Seq("and", a: Boolean, b: Boolean)     => a && b
          case Seq("or", a: Boolean, b: Boolean)      => a || b
          case Seq("=>", a: Boolean, b: Boolean)      => !a || b
          case Seq("not", b: Boolean)                 => !b
          case Seq(s: String)                         => input(s)
          case Seq(v: Int)                            => v
          case Seq(v: Boolean)                        => v //?
          case instr @ _                              => throw new Exception("Invalid instruction: " + instr)
        }
      }
    }
  }
}


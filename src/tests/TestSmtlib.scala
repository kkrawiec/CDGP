package tests

import cdgp.SMTLIBFormatter
import org.junit.Test
import org.junit.Assert._
import swim.tree.Op

final class TestSmtlib {
  @Test
  def test_opToString(): Unit = {
    val op = Op.fromStr("ite(>=(x y) x 0)", useSymbols=false)
    assertEquals("(ite (>= x y) x 0)", SMTLIBFormatter.opToString(op))
    assertEquals("(ite (>= x y) x 0)", SMTLIBFormatter(op))
  }
}

package tests

import java.io.File

import org.junit.Test
import org.junit.Assert._
import cdgp._
import fuel.func.RunExperiment
import fuel.util.IApp
import swim.tree.{CodeFactory, Op, SimpleGP}
import sygus.{BoolSortExpr, IntSortExpr, VarDeclCmd}



final class TestDomains {
  @Test
  def test_SLIA(): Unit = {
    val domain = DomainSLIA(List("x", "y", "z"), 'rec)
    val inputs = Seq(2, 10, 6)
    val inputs2 = Seq("a\"\"b", 10, 6)
    val op = Op('nt, 'x)
    assertEquals(2, domain(op)(inputs).get)
    val op2 = Op('nt, '+, Op('nt, 3), Op('nt, 'z))
    assertEquals(9, domain(op2)(inputs).get)
    val op3 = Op.fromStr("ite(<=(x 0) 0 +(2 rec(-(x 1) y z)))", useSymbols = true)
    assertEquals(4, domain(op3)(inputs).get)
    assertEquals("b", domain(Op.fromStr("str.at(x 2)", useSymbols = true))(inputs2).get)
    assertEquals("\"", domain(Op.fromStr("str.at(x 1)", useSymbols = true))(inputs2).get)

    val semantics = domain.operationalSemantics(Seq()) _

    // More information about String in CVC4: http://cvc4.cs.stanford.edu/wiki/Strings
    // Script to verify below expected answers: resources/str_test.smt2

    // str.prefixof
    assertEquals(true, semantics(Seq(Symbol("str.prefixof"), "", "")))
    assertEquals(true, semantics(Seq(Symbol("str.prefixof"), "", "asd")))
    assertEquals(false, semantics(Seq(Symbol("str.prefixof"), "asd", "")))
    assertEquals(true, semantics(Seq(Symbol("str.prefixof"), "asd", "asd")))
    assertEquals(true, semantics(Seq(Symbol("str.prefixof"), "asd", "asda")))
    assertEquals(false, semantics(Seq(Symbol("str.prefixof"), "da", "asda")))

    // str.suffixof
    assertEquals(true, semantics(Seq(Symbol("str.suffixof"), "", "")))
    assertEquals(true, semantics(Seq(Symbol("str.suffixof"), "", "asd")))
    assertEquals(false, semantics(Seq(Symbol("str.suffixof"), "asd", "")))
    assertEquals(true, semantics(Seq(Symbol("str.suffixof"), "asd", "asd")))
    assertEquals(false, semantics(Seq(Symbol("str.suffixof"), "asd", "asda")))
    assertEquals(true, semantics(Seq(Symbol("str.suffixof"), "da", "asda")))

    // str.at
    assertEquals("", semantics(Seq(Symbol("str.at"), "", -1)))
    assertEquals("", semantics(Seq(Symbol("str.at"), "", 0)))
    assertEquals("", semantics(Seq(Symbol("str.at"), "", 1)))
    assertEquals("", semantics(Seq(Symbol("str.at"), "a", -1)))
    assertEquals("a", semantics(Seq(Symbol("str.at"), "a", 0)))
    assertEquals("", semantics(Seq(Symbol("str.at"), "a", 1)))
    assertEquals("b", semantics(Seq(Symbol("str.at"), "ab", 1)))

    // str.contains
    assertEquals(true, semantics(Seq(Symbol("str.contains"), "", "")))
    assertEquals(true, semantics(Seq(Symbol("str.contains"), "asda", "")))
    assertEquals(false, semantics(Seq(Symbol("str.contains"), "asda", "ad")))
    assertEquals(true, semantics(Seq(Symbol("str.contains"), "aaa", "aa")))
    assertEquals(false, semantics(Seq(Symbol("str.contains"), "asda", "z")))

    // str.replace: in SMTLIB only the *first* occurrence is replaced
    assertEquals("", semantics(Seq(Symbol("str.replace"), "", "", "")))
    assertEquals("", semantics(Seq(Symbol("str.replace"), "", "", "5")))
    assertEquals("x", semantics(Seq(Symbol("str.replace"), "x", "", "5")))
    assertEquals("5  ", semantics(Seq(Symbol("str.replace"), "&(A  ", "&(A", "5")))
    assertEquals("xx", semantics(Seq(Symbol("str.replace"), "xx", "", "5")))
    assertEquals("5a", semantics(Seq(Symbol("str.replace"), "aaa", "aa", "5")))
    assertEquals("5sda", semantics(Seq(Symbol("str.replace"), "asda", "a", "5")))
    assertEquals("$\\x00", semantics(Seq(Symbol("str.replace"), "$\\x00 \\x00", "$\\x00 \\x00", "$\\x00")))

    // str.indexof
    assertEquals( 0, semantics(Seq(Symbol("str.indexof"), "", "", 0)))
    assertEquals(-1, semantics(Seq(Symbol("str.indexof"), "", "", -1)))
    assertEquals(-1, semantics(Seq(Symbol("str.indexof"), "", "", 1)))
    assertEquals(-1, semantics(Seq(Symbol("str.indexof"), " ", "  ", 0)))
    assertEquals(-1, semantics(Seq(Symbol("str.indexof"), "a", "", -1)))
    assertEquals( 0, semantics(Seq(Symbol("str.indexof"), "a", "", 0)))
    assertEquals( 1, semantics(Seq(Symbol("str.indexof"), "a", "", 1)))
    assertEquals(-1, semantics(Seq(Symbol("str.indexof"), "a", "", 2)))
    assertEquals(-1, semantics(Seq(Symbol("str.indexof"), "aa", "", -1)))
    assertEquals( 0, semantics(Seq(Symbol("str.indexof"), "aa", "", 0)))
    assertEquals( 1, semantics(Seq(Symbol("str.indexof"), "aa", "", 1)))
    assertEquals( 2, semantics(Seq(Symbol("str.indexof"), "aa", "", 2)))
    assertEquals(-1, semantics(Seq(Symbol("str.indexof"), "aa", "", 3)))
    assertEquals(-1, semantics(Seq(Symbol("str.indexof"), "a", "b", 0)))
    assertEquals( 0, semantics(Seq(Symbol("str.indexof"), "a", "a", 0)))
    assertEquals(-1, semantics(Seq(Symbol("str.indexof"), "aaa", "a", -1)))
    assertEquals( 0, semantics(Seq(Symbol("str.indexof"), "aaa", "a", 0)))
    assertEquals( 1, semantics(Seq(Symbol("str.indexof"), "aaa", "a", 1)))
    assertEquals( 2, semantics(Seq(Symbol("str.indexof"), "aaa", "a", 2)))
    assertEquals(-1, semantics(Seq(Symbol("str.indexof"), "aaa", "a", 3)))
    assertEquals( 2, semantics(Seq(Symbol("str.indexof"), "ssaaa", "aa", 0)))
    assertEquals( 0, semantics(Seq(Symbol("str.indexof"), "aaa", "aa", 0)))
    assertEquals( 1, semantics(Seq(Symbol("str.indexof"), "aaa", "aa", 1)))
    assertEquals(-1, semantics(Seq(Symbol("str.indexof"), "aaa", "aa", 2)))
    assertEquals(-1, semantics(Seq(Symbol("str.indexof"), "asda", "aa", 0)))

    // str.substr
    assertEquals("", semantics(Seq(Symbol("str.substr"), "", -1, 2)))
    assertEquals("", semantics(Seq(Symbol("str.substr"), "asd", -1, 2)))
    assertEquals("", semantics(Seq(Symbol("str.substr"), "", 0, 0)))
    assertEquals("", semantics(Seq(Symbol("str.substr"), "", 0, 1)))
    assertEquals("", semantics(Seq(Symbol("str.substr"), "as", -1, -1)))
    assertEquals("as", semantics(Seq(Symbol("str.substr"), "asdfgh", 0, 2)))
    assertEquals("asdfgh", semantics(Seq(Symbol("str.substr"), "asdfgh", 0, 10)))
    assertEquals("fgh", semantics(Seq(Symbol("str.substr"), "asdfgh", 3, 10)))
    assertEquals("", semantics(Seq(Symbol("str.substr"), "asdfgh", 10, 10)))
    assertEquals("asdfgh", semantics(Seq(Symbol("str.substr"), "asdfgh", 0, 100)))
    assertEquals("", semantics(Seq(Symbol("str.substr"), "asdfgh", 0, -1)))
    assertEquals("", semantics(Seq(Symbol("str.substr"), "asdfgh", 3, 0)))
    assertEquals("f", semantics(Seq(Symbol("str.substr"), "asdfgh", 3, 1)))
    assertEquals("fg", semantics(Seq(Symbol("str.substr"), "asdfgh", 3, 2)))

    // str.to.int
    assertEquals(-1, semantics(Seq(Symbol("str.to.int"), "")))
    assertEquals(-1, semantics(Seq(Symbol("str.to.int"), "-123")))
    assertEquals(-1, semantics(Seq(Symbol("str.to.int"), "(- 123)")))
    assertEquals(1, semantics(Seq(Symbol("str.to.int"), "1")))
    assertEquals(1234567, semantics(Seq(Symbol("str.to.int"), "1234567")))

    // int.to.str
    assertEquals("0", semantics(Seq(Symbol("int.to.str"), 0)))
    assertEquals("", semantics(Seq(Symbol("int.to.str"), -123)))
    assertEquals("", semantics(Seq(Symbol("int.to.str"), -12)))
    assertEquals("1", semantics(Seq(Symbol("int.to.str"), 1)))
    assertEquals("1234567", semantics(Seq(Symbol("int.to.str"),1234567)))
  }

  @Test
  def test_Reals(): Unit = {
    val domain = DomainReals(List("x", "y", "z"), 'rec)
    val inputs = Seq(2.0, 10.0, 6.0)
    val op = Op('nt, 'x)
    assertEquals(2.0, domain(op)(inputs).get)
    val op2 = Op('nt, '+, Op('nt, 3.0), Op('nt, 'z))
    assertEquals(9.0, domain(op2)(inputs).get)
    val op3 = Op.fromStr("ite(<=(x 0.0) 0.0 +(2.0 rec(-(x 1.0) y z)))", useSymbols = true)
    assertEquals(4.0, domain(op3)(inputs).get)
    val op4 = Op.fromStr("/(4.0 2.0)", useSymbols = true)
    assertEquals(2.0, domain(op4)(inputs).get)
  }
}

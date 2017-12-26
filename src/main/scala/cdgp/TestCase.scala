package cdgp

abstract class TestCase[I, O](in: I, out: Option[O])
  extends Tuple2[I, Option[O]](in, out) {
  def input: I = _1
  def output: Option[O] = _2
  def isCompleteTest: Boolean
}


/**
  * A standard test case for GP.
  */
class CompleteTestCase[I, O](input: I, output: Option[O]) extends TestCase[I, O](input, output) {
  override val isCompleteTest = true
}

object CompleteTestCase {
  def apply[I,O](input: I, output: Option[O]): CompleteTestCase[I,O] = new CompleteTestCase(input, output)
}


/**
  * Test case for which there is no single correct output. In such a situation
  * GP cannot evaluate the program on this test by simple execution. A certain
  * solver or dedicated procedure is required.
  */
class IncompleteTestCase[I, O](input: I) extends TestCase[I, O](input, None) {
  override val isCompleteTest = false
}

object IncompleteTestCase{
  def apply[I,O](input: I): IncompleteTestCase[I,O] = new IncompleteTestCase(input)
}
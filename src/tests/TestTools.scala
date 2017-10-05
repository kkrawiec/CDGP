package tests

import cdgp.Tools
import org.junit.Test
import org.junit.Assert._

final class TestTools {
  @Test
  def testParsePropertiesFileEmpy(): Unit = {
    val text =
      """
        |
        |
      """.stripMargin
    val opt = Tools.parsePropertiesFile(text)
    assertEquals(Map(), opt.allOptions)
  }

  @Test
  def testParsePropertiesFile(): Unit = {
    val text =
      """CDGPoneTestPerIter = false
        |CDGPtestsRatio = 0.5
      """.stripMargin
    val opt = Tools.parsePropertiesFile(text)
    assertEquals(Map("CDGPoneTestPerIter"->"false", "CDGPtestsRatio"->"0.5"), opt.allOptions)
  }
}

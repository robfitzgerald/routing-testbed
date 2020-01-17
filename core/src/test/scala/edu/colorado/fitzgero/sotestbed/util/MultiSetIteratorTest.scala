package edu.colorado.fitzgero.sotestbed.util

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest

class MultiSetIteratorTest extends SoTestBedBaseTest {
  "MultiSetIterator" when {
    "called with a multiset" should {
      "iterate over all combinations" in {
        val multiSet: Array[Array[String]] = Array(Array("a", "b"), Array("1", "2"))
        val it: MultiSetIterator[String] = MultiSetIterator(multiSet)
        val result: Seq[Array[String]] = it.toList
        result.length should equal(4)
        result should contain (Array("a", "1"))
        result should contain (Array("a", "2"))
        result should contain (Array("b", "1"))
        result should contain (Array("b", "2"))
      }
    }
  }
}

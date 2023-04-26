package edu.colorado.fitzgero.sotestbed.util

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import scala.util.Random

class WeightedSamplingWithReplacementTest extends SoTestBedBaseTest {
  "sampling" when {
    "called with an empty population but positive k value" should {
      "return an empty vector" in {
        val rng    = new Random(0)
        val result = WeightedSamplingWithReplacement.run(rng, List.empty[(String, Double)], 4)
        result.length should equal(0)
      }
    }
    "called with a small population with only one member has non-zero weights" should {
      "return a random vector" in {
        val rng    = new Random(0)
        val input  = List(("a", 0.0), ("b", 1.0), ("c", 0.0))
        val result = WeightedSamplingWithReplacement.run(rng, input, 4)
        result should equal(Vector("b", "b", "b", "b"))
      }
    }
    "called with a population with non-uniform weights" should {
      "return the expected vector with the given random seed value" in {
        val rng         = new Random(0)
        val inputRaw    = List(("a", 3.0), ("b", 2.0), ("c", 1.0))
        val sum         = inputRaw.map { case (_, w) => w }.sum
        val input       = inputRaw.map { case (k, w) => (k, w / sum) }
        val result      = WeightedSamplingWithReplacement.run(rng, input, 30)
        val expectedStr = "babbbaacccaaaabcababcabbbcbcaa"
        val expected    = expectedStr.toVector.map { c => c.toString }
        result should equal(expected)
      }
    }
  }
}

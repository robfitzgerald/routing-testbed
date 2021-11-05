package edu.colorado.fitzgero.sotestbed.util

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest

class SummaryStatsTest extends SoTestBedBaseTest {
  "SummaryStats" when {
    "empty" should {
      "have a mean of zero and nothing else" in {
        val result = SummaryStats()
        result.mean should equal(0.0)
        result.standardDeviation should equal(None)
        result.skewness should equal(None)
        result.kurtosis should equal(None)
      }
    }
    "one observation" should {
      "have a mean with that value and nothing else" in {}
    }
    "two observations" should {
      "have a mean and stuff?" in {}
    }
    "three observations" should {
      "do something cool" in {}
    }
    "four observations" should {
      "be in righteous harmony with the universe?" in {}
    }
  }
}

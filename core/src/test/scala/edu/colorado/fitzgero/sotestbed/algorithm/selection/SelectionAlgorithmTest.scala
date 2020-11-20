package edu.colorado.fitzgero.sotestbed.algorithm.selection

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest

class SelectionAlgorithmTest extends SoTestBedBaseTest {

  "SelectionAlgorithm" when {
    "numCombinationsLessThanThreshold" when {
      "called with a sufficiently small input" should {
        "be true" in {
          val input  = Map(0 -> List(1, 2), 1 -> List(1, 2, 3), 2 -> List(1, 2, 3, 4)) // 24 combinations
          val result = SelectionAlgorithm.numCombinationsLessThanThreshold(input, 25)
          result should be(true)
        }
      }
      "called with a sufficiently large input" should {
        "be false" in {
          val input  = Map(0 -> List(1, 2), 1 -> List(1, 2, 3), 2 -> List(1, 2, 3, 4))
          val result = SelectionAlgorithm.numCombinationsLessThanThreshold(input, 23) // 24 combinations
          result should be(false)
        }
      }
    }
  }
}

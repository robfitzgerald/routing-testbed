package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import scala.util.Random

class AuctionDelayAllocationMetricTests extends SoTestBedBaseTest {
  import AuctionDelayAllocationMetric._
  "metric" when {
    "called with an agent with net zero diff" should {
      "give us an allocation of 1" in {
        // up 10, down 10, down 10, up 10 - net 0.0
        val reqTimeEstimates  = List(50.0, 60.0, 50.0, 40.0)
        val afterSelEstimates = List(60.0, 50.0, 40.0, 50.0)
        val finalTravelTime   = 50.0
        val result            = computeMetricValue(reqTimeEstimates, afterSelEstimates, finalTravelTime)
        result should equal(Right(1.0))
      }
    }
    "called with an agent with net positive diff" should {
      "give us an allocation of 1" in {
        // net diff of 15 seconds improvement (positive)
        val reqTimeEstimates  = List(50.0, 45.0, 40.0, 45.0)
        val afterSelEstimates = List(45.0, 40.0, 45.0, 35.0)
        val finalTravelTime   = 35.0
        val result            = computeMetricValue(reqTimeEstimates, afterSelEstimates, finalTravelTime)
        result should equal(Right(1.0))
      }
    }
    "called with an agent with net negative diff where the magnitude does not exceed the final travel time" should {
      "give us an allocation in [0, 1]" in {
        // net diff of 15 seconds delay (negative)
        val reqTimeEstimates  = List(50.0, 55.0, 60.0, 55.0)
        val afterSelEstimates = List(55.0, 60.0, 55.0, 65.0)
        val finalTravelTime   = 65.0
        val result            = computeMetricValue(reqTimeEstimates, afterSelEstimates, finalTravelTime)
        result match {
          case Left(err) => throw err
          case Right(m) =>
            val expected = 1.0 - (15.0 / finalTravelTime)
            m should equal(expected +- 0.0001)
        }
      }
    }
    "called with an agent with net negative diff where the magnitude exceeds the final travel time" should {
      "give us an allocation of 0" in {
        // net diff of 15 seconds delay (negative)
        val reqTimeEstimates  = List(50.0, 55.0, 60.0, 55.0)
        val afterSelEstimates = List(55.0, 60.0, 55.0, 120.0)
        val finalTravelTime   = 65.0
        val result            = computeMetricValue(reqTimeEstimates, afterSelEstimates, finalTravelTime)
        result should equal(Right(0.0))
      }
    }
    "called with an agent that is net positive but their final estimate is longer than expected" should {
      "give us an allocation of 1" in {
        // net diff of 15 seconds improvement (positive)
        val reqTimeEstimates  = List(50.0, 45.0, 40.0, 45.0)
        val afterSelEstimates = List(45.0, 40.0, 45.0, 35.0)
        val finalTravelTime   = 45.0
        val result            = computeMetricValue(reqTimeEstimates, afterSelEstimates, finalTravelTime)
        result should equal(Right(1.0))
      }
    }
    "called with an agent that is net positive but their final estimate is shorter than expected" should {
      "give us an allocation of 1" in {
        // net diff of 15 seconds improvement (positive)
        val reqTimeEstimates  = List(50.0, 45.0, 40.0, 45.0)
        val afterSelEstimates = List(45.0, 40.0, 45.0, 35.0)
        val finalTravelTime   = 25.0
        val result            = computeMetricValue(reqTimeEstimates, afterSelEstimates, finalTravelTime)
        result should equal(Right(1.0))
      }
    }
    "called with an agent that is net negative but their final estimate is longer than expected" should {
      "change the value in a predictable way" in {
        // net diff of 15 seconds delay (negative)
        val reqTimeEstimates  = List(50.0, 55.0, 60.0, 55.0)
        val afterSelEstimates = List(55.0, 60.0, 55.0, 65.0)
        val finalTravelTime   = 80.0
        val result            = computeMetricValue(reqTimeEstimates, afterSelEstimates, finalTravelTime)
        result match {
          case Left(err) => throw err
          case Right(m) =>
            val expected = 1.0 - (15.0 / finalTravelTime)
            m should equal(expected +- 0.0001)
        }
      }
    }
    "called with an agent that is net negative but their final estimate is shorter than expected" should {
      "change the value in a predictable way" in {
        // net diff of 15 seconds delay (negative)
        val reqTimeEstimates  = List(50.0, 55.0, 60.0, 55.0)
        val afterSelEstimates = List(55.0, 60.0, 55.0, 65.0)
        val finalTravelTime   = 50.0
        val result            = computeMetricValue(reqTimeEstimates, afterSelEstimates, finalTravelTime)
        result match {
          case Left(err) => throw err
          case Right(m) =>
            val expected = 1.0 - (15.0 / finalTravelTime)
            m should equal(expected +- 0.0001)
        }
      }
    }
    "called with estimates with different lengths" should {
      "produce an error" in {
        val reqTimeEstimates  = List(50.0, 60.0)
        val afterSelEstimates = List(60.0, 50.0, 40.0, 50.0)
        val finalTravelTime   = 50.0
        val result            = computeMetricValue(reqTimeEstimates, afterSelEstimates, finalTravelTime)
        result.isLeft should be(true)
      }
    }
  }
}

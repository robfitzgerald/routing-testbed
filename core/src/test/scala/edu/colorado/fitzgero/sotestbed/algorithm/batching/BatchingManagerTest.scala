package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import org.scalatest.{Matchers, WordSpec}

class BatchingManagerTest extends WordSpec with Matchers {

  "BatchingManagerTest" when {

    "nextValidBatchingTime" when {
      "called with a 5 second batch window" should {
        "properly set the next valid batching time across a range of values" in {
          val batchWindow = SimTime(5)
          // the next batching time should be 5
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(-1)) should be (SimTime(5))

          // the next batching time should be 10
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(0)) should be (SimTime(10))
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(1)) should be (SimTime(10))
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(2)) should be (SimTime(10))
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(3)) should be (SimTime(10))
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(4)) should be (SimTime(10))

          // the next batching time should be 15
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(5)) should be (SimTime(15))
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(6)) should be (SimTime(15))
        }
      }
    }

    "listInvalidStrategies" when {
      "called with a 5 second batch window and a few invalid updates" should {
        "find an invalid batching time" in {
          val newStrategy: Map[SimTime, List[List[String]]] =
            Map(
              SimTime(10) -> List(List("7a", "7b"), List("7c", "7d")), // ok
              SimTime(5) -> List(List("3a", "3b"), List("3c", "3d")), // too soon
            )
          val batchWindow: SimTime = SimTime(5)
          val currentTime: SimTime = SimTime(3)
          val result: Seq[SimTime] = BatchingManager.listInvalidStrategies(newStrategy, batchWindow, currentTime)
          result.size should be (1)
        }
      }
    }

  }
}

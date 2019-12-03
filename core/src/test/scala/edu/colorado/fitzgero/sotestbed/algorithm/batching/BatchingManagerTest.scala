package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import org.scalatest.{Matchers, WordSpec}

class BatchingManagerTest extends WordSpec with Matchers {

  "BatchingManagerTest" when {

    "nextValidBatchingTime" when {
      "called with batch windows" should {
        "produce the correct result" in {
          val batchWindow = SimTime(5)
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(-1)) should be (SimTime(5))
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(0)) should be (SimTime(10))
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(1)) should be (SimTime(10))
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(2)) should be (SimTime(10))
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(3)) should be (SimTime(10))
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(4)) should be (SimTime(10))
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(5)) should be (SimTime(15))
          BatchingManager.nextValidBatchingTime(batchWindow, SimTime(6)) should be (SimTime(15))
        }
      }
    }

  }
}

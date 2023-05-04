package edu.colorado.fitzgero.sotestbed.matsim.simulator.flowhandler

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import org.matsim.api.core.v01.Id
import org.matsim.vehicles.Vehicle

class RoadNetworkFlowRecordTest extends SoTestBedBaseTest {
  "udpateAndCollect" when {
    "record is empty" should {
      "produce an unchanged state and the expected observation" in {
        val emptyRecord                  = RoadNetworkFlowRecord(linkLengthMeters = 100.0)
        val emptyObservation             = RoadNetworkFlowObservation(0, None, None)
        var arbitraryTime                = SimTime(30)
        val (updatedRecord, observation) = emptyRecord.updateAndCollect(arbitraryTime)
        updatedRecord should equal(emptyRecord)
        observation should equal(emptyObservation)
      }
    }
    "record has a single traversals in the specified time range" should {
      "updates the record and produce an observation that matches the observation" in {
        val vehicleId: Id[Vehicle]  = Id.create("testVehicle", classOf[Vehicle])
        val enterTime: SimTime      = SimTime(5)
        val exitTime: SimTime       = SimTime(65)
        val linkLength: Double      = 100.0
        val duration: SimTime       = exitTime - enterTime
        val durationSeconds: Double = duration.value.toDouble
        val speedMps: Double        = linkLength / durationSeconds
        val record =
          RoadNetworkFlowRecord(linkLength)
            .processLinkEnter(vehicleId, enterTime, None)
            .processLinkExit(vehicleId, exitTime, None)

        val binStartTime: SimTime = SimTime(0)
        val expectedRecord = record.copy(
          mostRecentAverageTraversalDurationSeconds = Some(durationSeconds),
          mostRecentAverageTraversalSpeedMps = Some(speedMps)
        )
        val expectedObservation =
          RoadNetworkFlowObservation(flowCount = 0, Some(durationSeconds), Some(speedMps))
        val (updatedRecord, observation) = record.updateAndCollect(binStartTime)

        updatedRecord should equal(expectedRecord)
        observation should equal(expectedObservation)
      }
    }
    "record has two traversals in the specified time range" should {
      "update the record and produce the expected observation" in {
        object V1 {
          val id: Id[Vehicle] = Id.create("v1", classOf[Vehicle])
          val enter: SimTime  = SimTime(5)
          val exit: SimTime   = SimTime(40)
          val dur: SimTime    = exit - enter
          val durSecs: Double = dur.value.toDouble
        }

        object V2 {
          val id: Id[Vehicle] = Id.create("v2", classOf[Vehicle])
          val enter: SimTime  = SimTime(10)
          val exit: SimTime   = SimTime(53)
          val dur: SimTime    = exit - enter
          val durSecs: Double = dur.value.toDouble
        }

        val record =
          RoadNetworkFlowRecord(linkLengthMeters = 100.0)
            .processLinkEnter(V1.id, V1.enter, None)
            .processLinkEnter(V2.id, V2.enter, None)
            .processLinkExit(V1.id, V1.exit, None)
            .processLinkExit(V2.id, V2.exit, None)

        val binStartTime: SimTime = SimTime(0)
        val avgDurSecs            = (V1.durSecs + V2.durSecs) / 2.0 // 39 seconds
        val avgSpdMps             = (record.linkLengthMeters / V1.durSecs + record.linkLengthMeters / V2.durSecs) / 2.0
        val expectedRecord = record.copy(
          mostRecentAverageTraversalDurationSeconds = Some(avgDurSecs)
        )
        val expectedObservation =
          RoadNetworkFlowObservation(0, Some(avgDurSecs), Some(avgSpdMps))
        val (updatedRecord, observation) = record.updateAndCollect(binStartTime)

        updatedRecord should equal(expectedRecord)
        observation should equal(expectedObservation)
      }
    }
    "record has one traversal in the specified time range and one stale traversal" should {
      "remove the stale traversal from the updated record and produce the expected observation" in {
        object V1 {
          val id: Id[Vehicle] = Id.create("v1", classOf[Vehicle])
          val enter: SimTime  = SimTime(5)
          val exit: SimTime   = SimTime(40)
          val dur: SimTime    = exit - enter
          val durSecs: Double = dur.value.toDouble
        }

        object V2 {
          val id: Id[Vehicle] = Id.create("v2", classOf[Vehicle])
          val enter: SimTime  = SimTime(15)
          val exit: SimTime   = SimTime(35)
          val dur: SimTime    = exit - enter
          val durSecs: Double = dur.value.toDouble
        }

        val record =
          RoadNetworkFlowRecord(linkLengthMeters = 100.0)
            .processLinkEnter(V1.id, V1.enter)
            .processLinkEnter(V2.id, V2.enter)
            .processLinkExit(V1.id, V1.exit)
            .processLinkExit(V2.id, V2.exit)

        val binStartTime: SimTime = SimTime(10)
        val expectedDuration      = V2.durSecs

        // v1 omitted from the expected record because it's stale
        val expectedRecord = record.copy(
          completedTraversals = List(RoadNetworkLinkTraversal(V2.enter, V2.dur)),
          mostRecentAverageTraversalDurationSeconds = Some(expectedDuration)
        )
        val expectedObservation =
          RoadNetworkFlowObservation(flowCount = 0, averageTraversalDurationSeconds = Some(expectedDuration))
        val (updatedRecord, observation) = record.updateAndCollect(binStartTime)

        updatedRecord should equal(expectedRecord)
        observation should equal(expectedObservation)
      }
    }
    "record has one traversal in the specified time range and one active traversal" should {
      "update the record and produce the expected observation" in {
        object V1 {
          val id: Id[Vehicle] = Id.create("v1", classOf[Vehicle])
          val enter: SimTime  = SimTime(5)
          val exit: SimTime   = SimTime(40)
          val dur: SimTime    = exit - enter
          val durSecs: Double = dur.value.toDouble
        }

        object V2 {
          val id: Id[Vehicle] = Id.create("v2", classOf[Vehicle])
          val enter: SimTime  = SimTime(15)
        }

        val record =
          RoadNetworkFlowRecord(linkLengthMeters = 100.0)
            .processLinkEnter(V1.id, V1.enter)
            .processLinkEnter(V2.id, V2.enter)
            .processLinkExit(V1.id, V1.exit)

        val binStartTime: SimTime = SimTime(0)
        val expectedDuration      = V1.durSecs

        // v1 omitted from the expected record because it's stale
        val expectedRecord = record.copy(
          mostRecentAverageTraversalDurationSeconds = Some(expectedDuration)
        )
        val expectedObservation =
          RoadNetworkFlowObservation(flowCount = 1, averageTraversalDurationSeconds = Some(expectedDuration))
        val (updatedRecord, observation) = record.updateAndCollect(binStartTime)

        updatedRecord should equal(expectedRecord)
        observation should equal(expectedObservation)
      }
    }
    "end to end test where one vehicle enters during one bin and exits during another" should {
      "correctly represent state at the two bin boundaries" in {
        object V1 {
          val id: Id[Vehicle] = Id.create("v1", classOf[Vehicle])
          val enter: SimTime  = SimTime(5)
          val exit: SimTime   = SimTime(40)
          val dur: SimTime    = exit - enter
          val durSecs: Double = dur.value.toDouble
        }

        object V2 {
          val id: Id[Vehicle] = Id.create("v2", classOf[Vehicle])
          val enter: SimTime  = SimTime(31)
          val exit: SimTime   = SimTime(70)
          val dur: SimTime    = exit - enter
          val durSecs: Double = dur.value.toDouble
        }

        val bin1: SimTime = SimTime(0)
        val bin2: SimTime = SimTime(30)

        val r0 =
          RoadNetworkFlowRecord(linkLengthMeters = 100.0)
            .processLinkEnter(V1.id, V1.enter)
            .processLinkEnter(V2.id, V2.enter)
            .processLinkExit(V1.id, V1.exit)

        val (r1, obs1) = r0.updateAndCollect(bin1)
        val r2         = r1.processLinkExit(V2.id, V2.exit)
        val (r3, obs2) = r2.updateAndCollect(bin2)

        r0.currentAgentCount should equal(1) // V2 still on link
        r1.currentAgentCount should equal(1)
        r2.currentAgentCount should equal(0) // V2 exits link
        r3.currentAgentCount should equal(0)

        r0.linkEnterTimes.get(V1.id) should equal(None)
        r0.linkEnterTimes.get(V2.id) should equal(Some(V2.enter))
        r1.linkEnterTimes.get(V2.id) should equal(Some(V2.enter))
        r2.linkEnterTimes.get(V2.id) should equal(None) // V2 exits link
        r3.linkEnterTimes.get(V2.id) should equal(None)

        r0.completedTraversals.length should be(1) // V1 completed
        r1.completedTraversals.length should be(1)
        r2.completedTraversals.length should be(2) // V1 and V2 completed
        r3.completedTraversals.length should be(1) // V1 stale, removed

        r0.mostRecentAverageTraversalDurationSeconds should equal(None)
        r1.mostRecentAverageTraversalDurationSeconds should equal(Some(V1.durSecs))
        r2.mostRecentAverageTraversalDurationSeconds should equal(Some(V1.durSecs))
        r3.mostRecentAverageTraversalDurationSeconds should equal(Some(V2.durSecs))

      }
    }
  }
}

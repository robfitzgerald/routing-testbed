package edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.model.numeric.{
  Capacity,
  Flow,
  Meters,
  MetersPerSecond,
  NonNegativeNumber,
  SimTime
}
import org.scalactic.{Equality, TolerantNumerics}

class EdgeBPRUpdateOpsTest extends SoTestBedBaseTest {

  "EdgeBPRUpdateOpsTest" when {

    "edgeUpdateWithFlowRate" when {
      "called on a range of values" should {
        "produce the correct (running mean) outcome" in {
          val fn: (EdgeBPR, Flow) => EdgeBPR = EdgeBPRUpdateOps.edgeUpdateWithFlowRate(SimTime(5))
          val edge: EdgeBPR                  = EdgeBPR(Meters(10), MetersPerSecond(10), MetersPerSecond(10), Capacity(1))
          val (f1, f2, f3, f4, f5, f6)       = (Flow(1), Flow(1), Flow(-1), Flow(1), Flow(-1), Flow(1))

          // sequence 6 updates.
          val edge1: EdgeBPR = fn(edge, f1)
          val edge2: EdgeBPR = fn(edge1, f2)
          val edge3: EdgeBPR = fn(edge2, f3)
          val edge4: EdgeBPR = fn(edge3, f4)
          val edge5: EdgeBPR = fn(edge4, f5)
          val edge6: EdgeBPR = fn(edge5, f6)

          // the count of flows should be correctly updated at each time step
          edge1.vehicleCount should equal(Flow(1)) // 0 + 1
          edge2.vehicleCount should equal(Flow(2)) // 0 + 1 + 1
          edge3.vehicleCount should equal(Flow(1)) // 0 + 1 + 1 - 1
          edge4.vehicleCount should equal(Flow(2)) // 0 + 1 + 1 - 1 + 1
          edge5.vehicleCount should equal(Flow(1)) // 0 + 1 + 1 - 1 + 1 - 1
          edge6.vehicleCount should equal(Flow(2)) // 0 + 1 + 1 - 1 + 1 - 1 + 1

          // the average flows observed over the buffer time should be correctly represented
          // (starts with a default value of Flow.Zero)
          edge1.flow.value should equal(Flow(0.5).value) // (0 + 1)             / 2 = 0.5
          edge2.flow.value should equal(Flow(1.0).value) // (0 + 1 + 2)         / 3 = 1.0
          edge3.flow.value should equal(Flow(1.0).value) // (0 + 1 + 2 + 1)     / 4 = 1.0
          edge4.flow.value should equal(Flow(1.2).value) // (0 + 1 + 2 + 1 + 2) / 5 = 1.2
          edge5.flow.value should equal(Flow(1.4).value) // (1 + 2 + 1 + 2 + 1) / 5 = 1.4 - dropped 1st value
          edge6.flow.value should equal(Flow(1.6).value) // (2 + 1 + 2 + 1 + 2) / 5 = 1.6 - dropped 2nd value
        }
      }

      "with a bunch of zeroes in the mix" should {
        "produce the correct (running mean) outcome" in {
          val fn: (EdgeBPR, Flow) => EdgeBPR = EdgeBPRUpdateOps.edgeUpdateWithFlowRate(SimTime(5))
          val edge: EdgeBPR                  = EdgeBPR(Meters(10), MetersPerSecond(10), MetersPerSecond(10), Capacity(1))
          val (f1, f2, f3, f4, f5, f6)       = (Flow(1), Flow(0), Flow(0), Flow(-1), Flow(0), Flow(0))

          // sequence 6 updates.
          val edge1: EdgeBPR = fn(edge, f1)
          val edge2: EdgeBPR = fn(edge1, f2)
          val edge3: EdgeBPR = fn(edge2, f3)
          val edge4: EdgeBPR = fn(edge3, f4)
          val edge5: EdgeBPR = fn(edge4, f5)
          val edge6: EdgeBPR = fn(edge5, f6)

          // the count of flows should be correctly updated at each time step
          edge1.vehicleCount should equal(Flow(1)) // 0 + 1
          edge2.vehicleCount should equal(Flow(1)) // 0 + 1 + 0
          edge3.vehicleCount should equal(Flow(1)) // 0 + 1 + 0 + 0
          edge4.vehicleCount should equal(Flow(0)) // 0 + 1 + 0 + 0 - 1
          edge5.vehicleCount should equal(Flow(0)) // 0 + 1 + 0 + 0 - 1 + 0
          edge6.vehicleCount should equal(Flow(0)) // 0 + 1 + 0 + 0 - 1 + 0 + 0

          val epsilon: Double                     = 0.001
          implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(epsilon)

          // the average flows observed over the buffer time should be correctly represented
          // (starts with a default value of Flow.Zero)
          edge1.flow.value should equal(Flow(0.5).value)   // (0 + 1)             / 2 = 0.5
          edge2.flow.value should equal(Flow(0.666).value) // (0 + 1 + 1)         / 3 = 0.666
          edge3.flow.value should equal(Flow(0.75).value)  // (0 + 1 + 1 + 1)     / 4 = 0.75
          edge4.flow.value should equal(Flow(0.6).value)   // (0 + 1 + 1 + 1 + 0) / 5 = 0.6
          edge5.flow.value should equal(Flow(0.6).value)   // (1 + 1 + 1 + 0 + 1) / 5 = 0.6 - dropped 1st value
          edge6.flow.value should equal(Flow(0.4).value)   // (0 + 0 + 0 + 1 + 1) / 5 = 0.4 - dropped 2nd value
        }
      }

      "with enough zeroes" should {
        "should trail out to zero flow" in {
          val fn: (EdgeBPR, Flow) => EdgeBPR = EdgeBPRUpdateOps.edgeUpdateWithFlowRate(SimTime(5))
          val edge: EdgeBPR                  = EdgeBPR(Meters(10), MetersPerSecond(10), MetersPerSecond(10), Capacity(1))
          val (f1, f2, f3, f4, f5, f6, f7)   = (Flow(1), Flow(-1), Flow(0), Flow(0), Flow(0), Flow(0), Flow(0))

          // sequence 6 updates.
          val edge1: EdgeBPR = fn(edge, f1)
          val edge2: EdgeBPR = fn(edge1, f2)
          val edge3: EdgeBPR = fn(edge2, f3)
          val edge4: EdgeBPR = fn(edge3, f4)
          val edge5: EdgeBPR = fn(edge4, f5)
          val edge6: EdgeBPR = fn(edge5, f6)
          val edge7: EdgeBPR = fn(edge6, f7)

          // the count of flows should be correctly updated at each time step
          edge1.vehicleCount should equal(Flow(1)) // 0 + 1
          edge2.vehicleCount should equal(Flow(0)) // 0 + 1 - 1
          edge3.vehicleCount should equal(Flow(0)) // 0 + 1 - 1 + 0
          edge4.vehicleCount should equal(Flow(0)) // 0 + 1 - 1 + 0 + 0
          edge5.vehicleCount should equal(Flow(0)) // 0 + 1 - 1 + 0 + 0 + 0
          edge6.vehicleCount should equal(Flow(0)) // 0 + 1 - 1 + 0 + 0 + 0 + 0
          edge7.vehicleCount should equal(Flow(0)) // 0 + 1 - 1 + 0 + 0 + 0 + 0 + 0

          val epsilon: Double                     = 0.001
          implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(epsilon)

          // the average flows observed over the buffer time should be correctly represented
          // (starts with a default value of Flow.Zero)
          edge1.flow.value should equal(Flow(0.5).value)   // (0 + 1)             / 2 = 0.5
          edge2.flow.value should equal(Flow(0.333).value) // (0 + 1 + 0)         / 3 = 0.333
          edge3.flow.value should equal(Flow(0.25).value)  // (0 + 1 + 0 + 0)     / 4 = 0.25
          edge4.flow.value should equal(Flow(0.2).value)   // (0 + 1 + 0 + 0 + 0) / 5 = 0.2
          edge5.flow.value should equal(Flow(0.2).value)   // (1 + 0 + 0 + 0 + 0) / 5 = 0.2 - dropped 1st value
          edge6.flow.value should equal(Flow(0.0).value)   // (0 + 0 + 0 + 0 + 0) / 5 = 0.0 - dropped 2nd value
        }
      }
    }
  }
}

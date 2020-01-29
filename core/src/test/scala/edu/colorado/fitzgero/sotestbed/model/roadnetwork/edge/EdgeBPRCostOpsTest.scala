package edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, Meters, MetersPerSecond, NonNegativeNumber, TravelTimeSeconds}

class EdgeBPRCostOpsTest extends SoTestBedBaseTest {

  "EdgeBPRCostOpsTest" should {

    "marginalCostFunction" ignore {

      // this is only a head check, not a formal test

      val fn: EdgeBPR => Flow => Cost = EdgeBPRCostOps.marginalCostFunction(0.15, 4.0)
      val edge: EdgeBPR = EdgeBPR(
        distance = Meters(100),
        freeFlowSpeed = MetersPerSecond(Meters(10), TravelTimeSeconds(1)),
        capacity = new NonNegativeNumber(10)
      )
      val costFlows: Flow => Cost = fn(edge)
      for {
        flow <- (0 to 20).map{Flow(_)}
      } {
        println(f"flow: ${flow.value}%.2f cost: ${costFlows(flow).value}%.2f")
      }
    }

  }
}

package edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.model.numeric.{
  Capacity,
  Cost,
  Flow,
  Meters,
  MetersPerSecond,
  NonNegativeNumber,
  SimTime,
  TravelTimeSeconds
}

class EdgeBPRCostOpsTest extends SoTestBedBaseTest {

  "EdgeBPRCostOpsTest" should {

    "marginalCostFunction".ignore {

      // this is only a head check, not a formal test

      val fn: EdgeBPR => Flow => Cost = EdgeBPRCostOps.marginalCostFunction(0.15, 4.0)
      val edge: EdgeBPR = EdgeBPR(
        distance = Meters(100),
        freeFlowSpeed = MetersPerSecond(Meters(10), TravelTimeSeconds(1)),
        observedSpeed = MetersPerSecond(Meters(10), TravelTimeSeconds(1)),
        capacity = Capacity(10)
      )
      val costFlows: Flow => Cost = fn(edge)
      for {
        flow <- (0 to 20).map { Flow(_) }
      } {
        println(f"flow: ${flow.value}%.2f cost: ${costFlows(flow).value}%.2f")
      }
    }

    "using matsim speeds" should {
      "produce reasonable values across feasible link capacity" in {
        // this is only a head check, not a formal test

        val fn: EdgeBPR => Flow => Cost = EdgeBPRCostOps.marginalCostFunction(0.15, 4.0)
        // 1 minute sim time + capacity-scaling window
        val timeStep: SimTime = SimTime(60)
        // 100 meter links
        val linkLength: Double = 100
        case class LinkType(
          name: String,
          freespeed: MetersPerSecond,
          capacity: Capacity,
          distance: Meters = Meters(linkLength)
        )
        val linkTypes = List(
          LinkType("motorway", MetersPerSecond(33.333333), Capacity(2000.0 * 2)),
          LinkType("motorway_link", MetersPerSecond(22.222222), Capacity(1500.0)),
          LinkType("trunk", MetersPerSecond(22.222222), Capacity(2000.0)),
          LinkType("trunk_link", MetersPerSecond(13.888888), Capacity(1500.0)),
          LinkType("primary", MetersPerSecond(22.222222), Capacity(1000.0)),
          LinkType("primary_link", MetersPerSecond(16.666666), Capacity(1000.0)),
          LinkType("secondary", MetersPerSecond(8.333333), Capacity(1000.0)),
          LinkType("secondary_link", MetersPerSecond(8.333333), Capacity(1000.0)),
          LinkType("tertiary", MetersPerSecond(6.944444), Capacity(600.0)),
          LinkType("tertiary_link", MetersPerSecond(6.944444), Capacity(600.0)),
          LinkType("unclassified", MetersPerSecond(4.166666), Capacity(600.0)),
          LinkType("residential", MetersPerSecond(4.166666), Capacity(600.0))
        )

        // print header
        println(linkTypes.map { _.name }.mkString("flow,", ",", ""))

        for {
          flow <- (0 to 20).map { Flow(_) }
        } {
          val linkTypesWithTravelTimes: List[Cost] = linkTypes.map { l =>
            val edgeCapacity     = Capacity((l.capacity.value * timeStep.value) / 3600.0)
            val edgeBPR          = EdgeBPR(l.distance, l.freespeed, l.freespeed, edgeCapacity, flow = flow)
            val travelTime: Cost = fn(edgeBPR)(Flow.Zero)
            travelTime
          }
          println(
            linkTypesWithTravelTimes
              .map { travelTime =>
                val mph: Double = (linkLength / travelTime.value) * (3600.0 / 1000.0)
//                f"${travelTime.value}%.2f"
                f"$mph%.2f"
              }
              .mkString(f"${flow.value},", ",", "")
          )
//          println(f"flow: ${flow.value}%.2f cost: ${costFlows(flow).value}%.2f")
        }
      }
    }

  }
}

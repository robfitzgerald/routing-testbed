package edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge

import scala.collection.immutable.Queue

import edu.colorado.fitzgero.sotestbed.model.numeric._

/**
  *
  * @param distance link distance, in meters
  * @param freeFlowSpeed free flow speed
  * @param observedSpeed speeds observed within the simulation at this time
  * @param capacity a strictly positive real for the max carrying capacity of vehicles on this edge
  * @param flow the current flow value from which cost flows will be calculated
  * @param flowHistory a Queue of [[Flow]]s, latest at the back; Queue offers constant/amoritized constant head/last ops
  * @param flowHistoryLength tracks length of Queue[Flow] to limit List traversals
  * @param vehicleCount tracks the actual count of vehicles at this time; may differ from "flow" which can be an average
  */
case class EdgeBPR(
  distance: Meters,
  freeFlowSpeed: MetersPerSecond,
  observedSpeed: MetersPerSecond,
  capacity: Capacity,
  flow: Flow = Flow.Zero,
  flowHistory: Queue[Flow] = Queue(Flow.Zero),
  flowHistoryLength: Int = 1,
  vehicleCount: Flow = Flow.Zero
) {
  lazy val freeFlowTravelTime: TravelTimeSeconds = Meters.toTravelTime(distance, freeFlowSpeed)
  lazy val observedTravelTime: TravelTimeSeconds = Meters.toTravelTime(distance, observedSpeed)
  override def toString: String                  = s"EdgeBPR(flow=$flow)"
}

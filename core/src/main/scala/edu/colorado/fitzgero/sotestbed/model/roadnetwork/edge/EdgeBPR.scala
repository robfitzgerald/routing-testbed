package edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge

import edu.colorado.fitzgero.sotestbed.model.numeric._

/**
  *
  * @param distance link distance, in meters
  * @param freeFlowSpeed free flow speed
  * @param capacity a strictly positive integer for the max carrying capacity of vehicles on this edge
  * @param flow the current flow value from which cost flows will be calculated
  * @param flowHistory a list of [[Flow]]s in reverse order (taking advantage of prepend [[List]] time efficiency)
  * @param flowHistoryLength tracks length of List[Flow] to limit List traversals
  */
case class EdgeBPR (
  distance: Meters,
  freeFlowSpeed: MetersPerSecond,
  capacity: NaturalNumber,
  flow: Flow = Flow.Zero,
  flowHistory: List[Flow] = List(Flow.Zero),
  flowHistoryLength: Int = 1
) {
  lazy val freeFlowTravelTime: TravelTimeSeconds = Meters.toTravelTime(distance, freeFlowSpeed)
  override def toString: String = s"EdgeBPR(flow=$flow)"
}


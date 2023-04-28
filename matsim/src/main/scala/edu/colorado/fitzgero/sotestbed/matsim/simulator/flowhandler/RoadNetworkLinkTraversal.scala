package edu.colorado.fitzgero.sotestbed.matsim.simulator.flowhandler

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

final case class RoadNetworkLinkTraversal(
  startTime: SimTime,
  duration: SimTime,
  startPos: LinkPosition,
  endPos: LinkPosition
)

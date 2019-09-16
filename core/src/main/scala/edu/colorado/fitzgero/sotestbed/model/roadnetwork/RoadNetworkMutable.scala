package edu.colorado.fitzgero.sotestbed.model.roadnetwork

import edu.colorado.fitzgero.sotestbed.model.numeric.Flow

trait RoadNetworkMutable[F[_], E] {
//  def updateEdgeFlow(edgeId: EdgeId, flow: Double): F[Unit]
  def updateEdgeFlows(flows: List[(EdgeId, Flow)], edgeUpdateFunction: (E, Flow) => E): F[Unit]
}

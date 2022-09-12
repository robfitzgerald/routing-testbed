package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.PathSegment
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost

/**
  * attributes associated with an Edge traversal
  * @param edgeId
  * @param estimatedTimeAtEdge observation of agent time at this edge. zero if the agent
  *                            has not yet traversed this edge
  * @param linkSourceCoordinate
  * @param linkDestinationCoordinate
  */
final case class EdgeData(
  edgeId: EdgeId,
  linkSourceCoordinate: Coordinate,
  linkDestinationCoordinate: Coordinate,
  linkDistance: Double,
  estimatedTimeAtEdge: Option[SimTime] = None
) {

  def toPathSegment: PathSegment = {
    val cost = this.estimatedTimeAtEdge match {
      case None      => Cost.Zero
      case Some(est) => Cost(est.value)
    }
    PathSegment(edgeId, cost)
  }
}

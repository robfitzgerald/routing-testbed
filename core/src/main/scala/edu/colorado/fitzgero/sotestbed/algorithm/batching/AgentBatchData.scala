package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

/**
  * captures the data about an agent's request which is meaningful for batch optimization and routing
  *
  * @param request a request for routing from the agent's current location to their destination
  * @param timeOfRequest the SimTime that the request was generated
  * @param currentEdgeRoute the edge ids for this request,
  *                         along with free flow travel time estimates for each link, and
  *                         the coordinate at the link's source vertex
  * @param lastReplanningTime the most recent replanning SimTime if the agent was previously re-planned
  */
final case class AgentBatchData(
  request: Request,
  timeOfRequest: SimTime,
  currentEdgeRoute: List[AgentBatchData.EdgeData],
  lastReplanningTime: Option[SimTime],
)

object AgentBatchData {
  final case class EdgeData(edgeId: EdgeId, estimatedTimeAtEdge: SimTime, linkSourceCoordinate: Coordinate, linkDestinationCoordinate: Coordinate)
}

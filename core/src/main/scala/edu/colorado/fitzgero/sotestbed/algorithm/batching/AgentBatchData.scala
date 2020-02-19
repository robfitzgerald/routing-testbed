package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Meters, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

sealed trait AgentBatchData

object AgentBatchData {

  /**
    * captures the data about an agent's request which is meaningful for batch optimization and routing
    *
    * @param request a request for routing from the agent's current location to their destination
    * @param timeOfRequest the SimTime that the request was generated
    * @param remainingRoute the edge ids for this request,
    *                         along with free flow travel time estimates for each link, and
    *                         the coordinate at the link's source vertex
    * @param lastReplanningTime the most recent replanning SimTime if the agent was previously re-planned
    */
  final case class RouteRequestData(
    request: Request,
    timeOfRequest: SimTime,
    experiencedRoute: List[RouteRequestData.EdgeData],
    remainingRoute: List[RouteRequestData.EdgeData],
    remainingRouteDistance: Meters,
    lastReplanningTime: Option[SimTime],
  ) extends AgentBatchData

  object RouteRequestData {
    final case class EdgeData(edgeId: EdgeId, estimatedTimeAtEdge: SimTime, linkSourceCoordinate: Coordinate, linkDestinationCoordinate: Coordinate)
  }

  /**
    * removes an agent from the batching manager (closes routing session)
    *
    * @param agentId the agent's id to remove
    */
  final case class SOAgentArrivalData(agentId: String) extends AgentBatchData
}

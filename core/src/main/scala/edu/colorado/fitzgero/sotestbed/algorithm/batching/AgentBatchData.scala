package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Meters, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, PathSegment}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

sealed trait AgentBatchData

object AgentBatchData {

  /**
    * captures the data about an agent's request which is meaningful for batch optimization and routing
    *
    * @param request a request for routing from the agent's current location to their destination
    * @param timeOfRequest the SimTime that the request was generated
    * @param experiencedRoute a route with experienced link traversal times
    * @param remainingRoute a route plan with no experienced times
    * @param lastReplanningTime the most recent replanning SimTime if the agent was previously re-planned
    */
  final case class RouteRequestData(
    request: Request,
    timeOfRequest: SimTime,
    experiencedRoute: List[RouteRequestData.EdgeData],
    remainingRoute: List[RouteRequestData.EdgeData],
    remainingRouteDistance: Meters,
    lastReplanningTime: Option[SimTime]
  ) extends AgentBatchData

  object RouteRequestData {

    /**
      * attributes associated with an Edge traversal
      * @param edgeId
      * @param estimatedTimeAtEdge
      * @param linkSourceCoordinate
      * @param linkDestinationCoordinate
      */
    final case class EdgeData(
      edgeId: EdgeId,
      linkSourceCoordinate: Coordinate,
      linkDestinationCoordinate: Coordinate,
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
  }

  /**
    * removes an agent from the batching manager (closes routing session)
    *
    * @param agentId the agent's id to remove
    */
  final case class SOAgentArrivalData(agentId: String) extends AgentBatchData
}

package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

/**
  * captures the data about an agent's request which is meaningful for batch optimization and routing
  *
  * @param request a request for routing from the agent's current location to their destination
  * @param timeOfRequest the SimTime that the request was generated
  * @param currentEdgeRoute the edge ids for this request
  * @param lastReplanningTime the most recent replanning SimTime if the agent was previously re-planned
  */
final case class AgentBatchData(
  request: Request,
  timeOfRequest: SimTime,
  currentEdgeRoute: List[EdgeId],
  lastReplanningTime: Option[SimTime],
)

package edu.colorado.fitzgero.sotestbed.matsim.simulator

import edu.colorado.fitzgero.sotestbed.algorithm.batching._
import org.matsim.api.core.v01.population.Leg
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import org.matsim.core.router.util.TravelTime
import org.matsim.core.mobsim.qsim.QSim
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.network.Link
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.agent.RequestClass
import edu.colorado.fitzgero.sotestbed.model.agent.RequestClass.SO
import edu.colorado.fitzgero.sotestbed.model.agent.RequestClass.UE
import com.typesafe.scalalogging.LazyLogging

final case class AgentTripData(
  experienced: List[EdgeData],
  remaining: List[EdgeData]
)

object AgentTripData extends LazyLogging {

  /**
    * collects the current agent's experienced trip and estimated remaining trip
    *
    * @param agentState the stateful data currently found in MATSim for this agent
    * @param leg the current agent trip leg
    * @param currentTime the current simulation time, in seconds
    * @param qSim the current queue simulation state
    * @return AgentTripData for the current agent
    */
  def collectUEAgentTripData(
    agentState: AgentState,
    leg: Leg,
    currentTime: Long,
    qSim: QSim
  ): AgentTripData = {
    // build Requests for this time step
    val currentLinkId     = agentState.mobsimAgent.getCurrentLinkId
    val fullRoute         = MATSimRouteOps.convertToCompleteRoute(leg)
    val experiencedRoute  = fullRoute.takeWhile(_ != currentLinkId)
    val remainingLinkIds  = fullRoute.dropWhile(_ != currentLinkId)
    val endLinkId         = leg.getRoute.getEndLinkId
    val destinationEdgeId = EdgeId(endLinkId.toString)

    // we don't track travel time for experienced links on these agents
    val experiencedEdgeData: List[EdgeData] = MATSimRouteOps.convertRouteToEdgeData(experiencedRoute, qSim)
    val remainingEdgeData: List[EdgeData]   = MATSimRouteOps.convertRouteToEdgeData(remainingLinkIds, qSim, None)
    AgentTripData(experiencedEdgeData, remainingEdgeData)
  }

  /**
    * collects the current agent's experienced trip and estimated remaining trip
    *
    * @param agentState the stateful data currently found in MATSim for this agent
    * @param agentData the aggregate data for this agent
    * @param leg the current agent trip leg
    * @param currentTime the current simulation time, in seconds
    * @param travelTimeOption if provided, the context required to estimate travel times
    * @param qSim the current queue simulation state
    * @return agent trip data
    */
  def collectSOAgentTripData(
    agentState: AgentState,
    agentData: AgentData,
    leg: Leg,
    currentTime: Long,
    travelTime: Id[Link] => SimTime,
    qSim: QSim
  ): AgentTripData = {
    // build Requests for this time step
    val currentLinkId     = agentState.mobsimAgent.getCurrentLinkId
    val fullRoute         = MATSimRouteOps.convertToCompleteRoute(leg)
    val experiencedRoute  = agentData.getExperiencedRoute
    val remainingLinkIds  = fullRoute.dropWhile(_ != currentLinkId)
    val destinationEdgeId = EdgeId(leg.getRoute.getEndLinkId.toString)

    // pull in observed travel times over links here if requested
    // val ttReq = MATSimRouteOps.EdgeDataRequestWithTravelTime(
    //   agentState.person,
    //   agentState.vehicle,
    //   SimTime(currentTime),
    //   travelTime
    // )
    val experiencedEdgeData: List[EdgeData] = MATSimRouteOps.convertExperiencedRouteToEdgeData(experiencedRoute, qSim)
    val remainingEdgeData: List[EdgeData] =
      MATSimRouteOps.convertRouteToEdgeData(remainingLinkIds, qSim, Some(travelTime))

    AgentTripData(experiencedEdgeData, remainingEdgeData)
  }

}

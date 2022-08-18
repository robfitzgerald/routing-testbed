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

final case class AgentTripData(
  experienced: List[EdgeData],
  remaining: List[EdgeData]
)

object AgentTripData {

  /**
    * collects the current agent's experienced trip and estimated remaining trip
    *
    * @param agentData the aggregate data for this agent
    * @param agentState the stateful data currently found in MATSim for this agent
    * @param leg the current agent trip leg
    * @param currentTime the current simulation time, in seconds
    * @param travelTimeOption if provided, the context required to estimate travel times
    * @param qSim the current queue simulation state
    * @return agent trip data
    */
  def collectAgentTripData(
    agentState: AgentState,
    experiencedRoute: List[(Id[Link], Cost)],
    leg: Leg,
    currentTime: Long,
    travelTimeOption: Option[TravelTime],
    qSim: QSim
  ) = {
    // build Requests for this time step
    val currentLinkId     = agentState.mobsimAgent.getCurrentLinkId
    val fullRoute         = MATSimRouteOps.convertToCompleteRoute(leg)
    val remainingLinkIds  = fullRoute.dropWhile(_ != currentLinkId)
    val endLinkId         = leg.getRoute.getEndLinkId
    val destinationEdgeId = EdgeId(endLinkId.toString)

    // get experienced and estimated travel times
    val ttRequest = travelTimeOption.map { ttc =>
      MATSimRouteOps.EdgeDataRequestWithTravelTime(
        agentState.person,
        agentState.vehicle,
        SimTime(currentTime),
        ttc
      )
    }

    val experiencedEdgeData: List[EdgeData] =
      MATSimRouteOps
        .convertExperiencedRouteToEdgeData(experiencedRoute, qSim)
    val remainingEdgeData: List[EdgeData] =
      MATSimRouteOps.convertRouteToEdgeData(
        remainingLinkIds,
        qSim,
        ttRequest
      )
    AgentTripData(experiencedEdgeData, remainingEdgeData)
  }

}

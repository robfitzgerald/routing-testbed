package edu.colorado.fitzgero.sotestbed.matsim.simulator

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData
import org.matsim.core.mobsim.qsim.QSim
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.population.Person
import org.matsim.vehicles.Vehicle
import edu.colorado.fitzgero.sotestbed.model.agent.RequestClass
import org.matsim.core.router.util.TravelTime
import edu.colorado.fitzgero.sotestbed.model.numeric.TravelTimeSeconds
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.model.agent.TravelMode
import edu.colorado.fitzgero.sotestbed.model.numeric.Meters
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

object GenerateRouteRequest {

  def apply(
    qSim: QSim,
    personId: Id[Person],
    vehicleId: Id[Vehicle],
    requestClass: RequestClass,
    timeEnteredVehicle: SimTime,
    travelTime: Option[TravelTime],
    mostRecentTimeReplanned: Option[SimTime],
    minimumReplanningLeadTime: TravelTimeSeconds,
    minimumRemainingRouteTimeForReplanning: TravelTimeSeconds
  ): Either[Error, Option[AgentBatchData.RouteRequestData]] = {
    val agentStateOrErr = AgentState.forAgent(qSim, personId, vehicleId)
    val currentTime     = qSim.getSimTimer.getTimeOfDay.toLong

    // get the experienced and estimated remaining trip plans
    val agentTripDataOrErr = for {
      as  <- agentStateOrErr
      leg <- as.getModifiableLeg
    } yield AgentTripData.collectAgentTripData(
      agentState = as,
      experiencedRoute = List.empty,
      leg = leg,
      currentTime = currentTime,
      travelTimeOption = travelTime,
      qSim = qSim
    )

    // find a link along the remaining trip plan that is suitable for
    // originating a replanning request
    val startLinkIdOrErr = for {
      as <- agentStateOrErr
      currentLinkId = as.mobsimAgent.getCurrentLinkId
      leg <- as.getModifiableLeg
    } yield {
      val fullRoute = MATSimRouteOps.convertToCompleteRoute(leg)
      val endLinkId = leg.getRoute.getEndLinkId
      MATSimRouteOps.selectRequestOriginLink(
        fullRoute,
        currentLinkId,
        endLinkId,
        qSim,
        minimumReplanningLeadTime,
        minimumRemainingRouteTimeForReplanning
      )
    }

    // create a request
    val reqOptOrErr = for {
      as                <- agentStateOrErr
      leg               <- as.getModifiableLeg
      startLinkIdOption <- startLinkIdOrErr
      endLinkId = leg.getRoute.getEndLinkId
    } yield {
      startLinkIdOption.map { startLinkId =>
        Request(
          agent = personId.toString,
          location = EdgeId(startLinkId.toString),
          destination = EdgeId(endLinkId.toString),
          requestClass = requestClass,
          travelMode = TravelMode.Car
        )
      }
    }

    // wrap the request in a RouteRequestData wrapper that
    // includes additional data about the request
    val result = for {
      agentTripData <- agentTripDataOrErr
      requestOption <- reqOptOrErr
    } yield requestOption.map { request =>
      val remainingDistance: Meters =
        MATSimRouteOps
          .distanceOfEdgeData(agentTripData.remaining, qSim)
          .getOrElse(Meters.Zero)

      val experiencedTravelTime = SimTime(currentTime) - timeEnteredVehicle

      AgentBatchData.RouteRequestData(
        request = request,
        timeOfRequest = SimTime(currentTime),
        experiencedTravelTime = experiencedTravelTime,
        experiencedRoute = agentTripData.experienced,
        remainingRoute = agentTripData.remaining,
        remainingRouteDistance = remainingDistance,
        lastReplanningTime = mostRecentTimeReplanned
      )
    }

    result
  }
}

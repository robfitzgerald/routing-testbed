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
import org.matsim.api.core.v01.network.Link
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.agent.RequestClass.SO
import edu.colorado.fitzgero.sotestbed.model.agent.RequestClass.UE
import cats.implicits._

object GenerateAgentData {

  /**
    * generates an EnterSimulation message
    *
    * @param qSim current sim state
    * @param travelTime travel time calculator
    * @param personId person entering
    * @param vehicleId vehicle entering
    * @param currentTime time agent enters sim
    */
  def generateEnterSimulation(
    qSim: QSim,
    travelTime: TravelTime,
    personId: Id[Person],
    vehicleId: Id[Vehicle],
    currentTime: SimTime
  ): Either[Error, AgentBatchData.EnterSimulation] = {
    val agentStateOrErr = AgentState.forAgent(qSim, personId, vehicleId)
    val agentData       = AgentData(personId, vehicleId, DepartureTime(currentTime.value.toInt))
    val agentTripDataOrError = for {
      as  <- agentStateOrErr
      leg <- as.getModifiableLeg
    } yield AgentTripData.collectSOAgentTripData(
      agentState = as,
      agentData = agentData,
      leg = leg,
      currentTime = currentTime.value,
      travelTime = travelTime,
      qSim = qSim
    )

    agentTripDataOrError.map { atd =>
      AgentBatchData.EnterSimulation(
        agent = personId.toString,
        departureTime = currentTime,
        initialRoute = atd.remaining
      )
    }
  }

  /**
    * translates the state for some agent from MATSim to a request to our routing service
    *
    * @param qSim the queue simulation
    * @param personId the person
    * @param vehicleId the person's vehicle, typically the same id
    * @param requestClass UE or SO agent
    * @param timeEnteredVehicle start of trip time
    * @param travelTime optional context for estimating travel time for the remaining trip
    * @param mostRecentTimeReplanned time in simulation when vehicle was last replanned, if any
    * @param minimumReplanningLeadTime global threshold for lead time between replanning events
    * @param minimumRemainingRouteTimeForReplanning global threshold for allowing replanning based on the amount
    *                                               of travel time left on this trip
    * @return either some optional request (None if not within thresholds) or an error
    */
  def generateRouteRequest(
    qSim: QSim,
    personId: Id[Person],
    vehicleId: Id[Vehicle],
    requestClass: RequestClass,
    timeEnteredVehicle: SimTime,
    travelTime: TravelTime,
    agentData: Option[AgentData],
    mostRecentTimeReplanned: Option[SimTime],
    minimumReplanningLeadTime: TravelTimeSeconds,
    minimumRemainingRouteTimeForReplanning: TravelTimeSeconds
  ): Either[Error, Option[AgentBatchData.RouteRequestData]] = {
    val agentStateOrErr = AgentState.forAgent(qSim, personId, vehicleId)
    val currentTime     = qSim.getSimTimer.getTimeOfDay.toLong

    // get the experienced and estimated remaining trip plans
    val agentTripDataOrErr: Either[Error, Either[Error, AgentTripData]] = for {
      as  <- agentStateOrErr
      leg <- as.getModifiableLeg
    } yield requestClass match {
      case _: SO =>
        agentData match {
          case None => Left(new Error(s"must invoke GenerateRouteRequest for SO agents with AgentData in payload"))
          case Some(ad) =>
            val atd = AgentTripData.collectSOAgentTripData(
              agentState = as,
              agentData = ad,
              leg = leg,
              currentTime = currentTime,
              travelTime = travelTime,
              qSim = qSim
            )
            Right(atd)
        }
      case UE =>
        val atd = AgentTripData.collectUEAgentTripData(
          agentState = as,
          leg = leg,
          currentTime = currentTime,
          qSim = qSim
        )
        Right(atd)
    }

    // find a link along the remaining trip plan that is suitable for
    // originating a replanning request
    val startLinkIdOrErr = for {
      as <- agentStateOrErr
      currentLinkId = as.mobsimAgent.getCurrentLinkId
      leg <- as.getModifiableLeg
    } yield {
      val fullRoute = MATSimRouteOps.convertToCompleteRoute(leg)
      val endLinkId = leg.getRoute.getEndLinkId
      val ttRequest =
        MATSimRouteOps.EdgeDataRequestWithTravelTime(
          as.person,
          as.vehicle,
          SimTime(currentTime),
          travelTime
        )
      MATSimRouteOps.selectRequestOriginLink(
        fullRoute,
        currentLinkId,
        endLinkId,
        qSim,
        ttRequest,
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
      agentTripData <- agentTripDataOrErr.flatten
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

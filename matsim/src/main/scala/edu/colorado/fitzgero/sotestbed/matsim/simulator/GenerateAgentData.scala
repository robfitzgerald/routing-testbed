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
import cats.effect.IO

sealed trait GenerateAgentData

object GenerateAgentData {

  final case class GenerateEnterSimulation(
    travelTime: Id[Link] => SimTime,
    personId: Id[Person],
    vehicleId: Id[Vehicle],
    currentTime: SimTime
  ) extends GenerateAgentData

  final case class GenerateRouteRequest(
    personId: Id[Person],
    vehicleId: Id[Vehicle],
    requestClass: RequestClass,
    timeEnteredVehicle: SimTime,
    travelTime: Id[Link] => SimTime,
    agentData: Option[AgentData],
    mostRecentTimeReplanned: Option[SimTime],
    minimumReplanningLeadTime: TravelTimeSeconds,
    minimumRemainingRouteTimeForReplanning: TravelTimeSeconds
  ) extends GenerateAgentData

  // def generateAgentData(
  //   qSim: QSim,
  //   args: GenerateAgentData
  // ): IO[AgentBatchData] = args match {
  //   case ges: GenerateEnterSimulation => generateEnterSimulation(qSim, ges)
  //   case grr: GenerateRouteRequest    => generateRouteRequest(qSim, grr)
  // }

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
    args: GenerateEnterSimulation
  ): IO[AgentBatchData] = {
    val agentStateOrErr = AgentState.forAgent(qSim, args.personId, args.vehicleId)
    val agentData       = AgentData(args.personId, args.vehicleId, DepartureTime(args.currentTime.value.toInt))
    val agentTripDataOrError = for {
      as  <- agentStateOrErr
      leg <- as.getModifiableLeg
    } yield AgentTripData.collectSOAgentTripData(
      agentState = as,
      agentData = agentData,
      leg = leg,
      currentTime = args.currentTime.value,
      travelTime = args.travelTime,
      qSim = qSim
    )

    val result = agentTripDataOrError.map { atd =>
      AgentBatchData.EnterSimulation(
        agent = args.personId.toString,
        departureTime = args.currentTime,
        initialRoute = atd.remaining
      )
    }
    IO.fromEither(result)
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
    args: GenerateRouteRequest
  ): IO[Option[AgentBatchData.RouteRequestData]] = {
    val agentStateOrErr = AgentState.forAgent(qSim, args.personId, args.vehicleId)
    val currentTime     = qSim.getSimTimer.getTimeOfDay.toLong

    // get the experienced and estimated remaining trip plans
    val agentTripDataOrErr: Either[Error, Either[Error, AgentTripData]] = for {
      as  <- agentStateOrErr
      leg <- as.getModifiableLeg.left.map { t => new Error(s"agent in state ${as.mobsimAgent.getState}", t) }
    } yield args.requestClass match {
      case _: SO =>
        args.agentData match {
          case None => Left(new Error(s"must invoke GenerateRouteRequest for SO agents with AgentData in payload"))
          case Some(ad) =>
            val atd = AgentTripData.collectSOAgentTripData(
              agentState = as,
              agentData = ad,
              leg = leg,
              currentTime = currentTime,
              travelTime = args.travelTime,
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
          args.travelTime
        )
      MATSimRouteOps.selectRequestOriginLink(
        fullRoute,
        currentLinkId,
        endLinkId,
        qSim,
        ttRequest,
        args.minimumReplanningLeadTime,
        args.minimumRemainingRouteTimeForReplanning
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
          agent = args.personId.toString,
          location = EdgeId(startLinkId.toString),
          destination = EdgeId(endLinkId.toString),
          requestClass = args.requestClass,
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

      val experiencedTravelTime = SimTime(currentTime) - args.timeEnteredVehicle

      AgentBatchData.RouteRequestData(
        request = request,
        timeOfRequest = SimTime(currentTime),
        experiencedTravelTime = experiencedTravelTime,
        experiencedRoute = agentTripData.experienced,
        remainingRoute = agentTripData.remaining,
        remainingRouteDistance = remainingDistance,
        lastReplanningTime = args.mostRecentTimeReplanned
      )
    }

    IO.fromEither(result)
  }
}

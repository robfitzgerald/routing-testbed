package edu.colorado.fitzgero.sotestbed.matsim.simulator

import cats.effect.IO
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData
import org.matsim.api.core.v01.Id
import org.matsim.vehicles.Vehicle
import org.matsim.api.core.v01.population.Person
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.numeric.TravelTimeSeconds
import edu.colorado.fitzgero.sotestbed.model.agent.RequestClass
import org.matsim.core.mobsim.qsim.QSim
import org.matsim.core.router.util.TravelTime
import scala.collection.JavaConverters._
import org.matsim.api.core.v01.network.Link
import com.typesafe.scalalogging.LazyLogging
import org.matsim.core.mobsim.framework.MobsimAgent.State
import org.matsim.core.trafficmonitoring.TravelTimeCalculator
import org.matsim.core.mobsim.framework.MobsimAgent

object GenerateAgentDataOps extends LazyLogging {

  /**
    * helper for generating UE route requests
    *
    * @param qSim
    * @param travelTime
    * @param minimumReplanningLeadTime
    * @param minimumRemainingRouteTimeForReplanning
    * @param vehicleId
    * @param personId
    * @param timeEnteredVehicle
    * @return
    */
  def generateUeAgentRouteRequest(
    qSim: QSim,
    travelTime: TravelTime,
    minimumReplanningLeadTime: TravelTimeSeconds,
    minimumRemainingRouteTimeForReplanning: TravelTimeSeconds
  )(vehicleId: Id[Vehicle], personId: Id[Person], timeEnteredVehicle: SimTime): IO[Option[AgentBatchData]] = {

    // UE agents have a need for a route "flagged" (/marked) when they enter the system,
    // but, if their route is short enough, then an agent may reach its destination before
    // their route request is submitted. to prevent this, we only proceed for agents that are
    // in a [[State.LEG]] (and not [[State.ACTIVITY]] which implies they have reached their destination).
    val enRouteResult =
      IO.fromEither(AgentState.forAgent(qSim, personId, vehicleId)).map { _.mobsimAgent.getState == State.LEG }

    // request a Dijkstra's shortest path for this agent

    val args = GenerateAgentData.GenerateRouteRequest(
      personId = personId,
      vehicleId = vehicleId,
      requestClass = RequestClass.UE,
      timeEnteredVehicle = timeEnteredVehicle,
      travelTime = travelTime,
      agentData = None,
      mostRecentTimeReplanned = None,
      minimumReplanningLeadTime = minimumReplanningLeadTime,
      minimumRemainingRouteTimeForReplanning = minimumRemainingRouteTimeForReplanning
    )
    enRouteResult.flatMap {
      case false => IO.pure(None)
      case true  => GenerateAgentData.generateRouteRequest(qSim, args)
    }
  }

  /**
    * helper for generating SO route requests
    *
    * @param qSim
    * @param replanningHandler
    * @param travelTime
    * @param minimumReplanningLeadTime
    * @param minimumRemainingRouteTimeForReplanning
    * @param agentData
    * @return
    */
  def generateSoAgentRouteRequest(
    qSim: QSim,
    replanningHandler: SOAgentReplanningHandler,
    travelTime: TravelTime,
    minimumReplanningLeadTime: TravelTimeSeconds,
    minimumRemainingRouteTimeForReplanning: TravelTimeSeconds
  )(agentData: AgentData): IO[Option[AgentBatchData]] = {
    val lastReplanningTime: Option[SimTime] =
      replanningHandler
        .getMostRecentTimePlannedForAgent(agentData.personId)

    val args = GenerateAgentData.GenerateRouteRequest(
      personId = agentData.personId,
      vehicleId = agentData.vehicleId,
      requestClass = RequestClass.SO(),
      timeEnteredVehicle = SimTime(agentData.timeEnteredVehicle.value),
      travelTime = travelTime,
      agentData = Some(agentData),
      mostRecentTimeReplanned = lastReplanningTime,
      minimumReplanningLeadTime = minimumReplanningLeadTime,
      minimumRemainingRouteTimeForReplanning = minimumRemainingRouteTimeForReplanning
    )

    // guard against submitting requests for agents that have already reached their
    // destination. possible when MATSim consumes more than one second at a time when stepped.
    // PlayPauseSimulationControl has that behavior unless we really throttle it.
    val result =
      IO.fromEither(AgentState.forAgent(qSim, agentData.personId, agentData.vehicleId))
        .flatMap {
          case as if as.mobsimAgent.getState == MobsimAgent.State.LEG =>
            GenerateAgentData.generateRouteRequest(qSim, args)
          case _ =>
            IO.pure(None)
        }

    result
  }

  /**
    * if we are not replanning, then we want to copy any existing plans
    * for this agent over to MATSim. this applies to both UE and SO agents.
    *
    * @param qSim
    * @param vehicleId
    * @param personId
    * @param completePathStore
    * @return
    */
  def applyStoredRoute(
    qSim: QSim,
    vehicleId: Id[Vehicle],
    personId: Id[Person],
    completePathStore: collection.mutable.Map[Id[Person], Map[DepartureTime, List[Id[Link]]]]
  ): IO[Unit] = {
    // find stored path for this agent/departure time combination if it exists
    val legAndStoredPathResult = for {
      mobsimAgent <- IO.fromOption(qSim.getAgents.asScala.get(personId))(new Error(s"person $personId not in QSim"))
      leg <- IO.fromOption(MATSimRouteOps.safeGetModifiableLeg(mobsimAgent))(
        new Error(s"unable to extract modifiable leg for $personId")
      )
      departureTime <- IO.fromOption(DepartureTime.getLegDepartureTime(leg))(new Error())
    } yield {
      // checks that there IS a path, and that it's reasonable to assign here
      val pathOption =
        completePathStore
          .getOrElse(personId, Map.empty[DepartureTime, List[Id[Link]]])
          .get(departureTime)

      pathOption.flatMap { path =>
        // todo: also run CoalescePath here to confirm we can combine the stored with the current path
        if (MATSimRouteOps.completePathHasAtLeastTwoLinks(path)) Some((leg, path))
        else None
      }
    }

    legAndStoredPathResult.map {
      case None              =>
      case Some((leg, path)) =>
        // replace the path in this leg of the trip with the stored path
        MATSimRouteOps.assignCompleteRouteToLeg(path, leg)
        logger.debug(
          s"agent $personId: applying stored route with ${path.length} edges"
        )
    }
  }

}

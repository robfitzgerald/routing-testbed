package edu.colorado.fitzgero.sotestbed.matsim.simulator

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, SimTime}
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.events.handler.{
  LinkEnterEventHandler,
  LinkLeaveEventHandler,
  PersonEntersVehicleEventHandler,
  PersonLeavesVehicleEventHandler,
  PersonStuckEventHandler,
  VehicleEntersTrafficEventHandler,
  VehicleLeavesTrafficEventHandler
}
import org.matsim.api.core.v01.events.{
  LinkEnterEvent,
  LinkLeaveEvent,
  PersonEntersVehicleEvent,
  PersonLeavesVehicleEvent,
  PersonStuckEvent,
  VehicleEntersTrafficEvent,
  VehicleLeavesTrafficEvent
}
import org.matsim.api.core.v01.network.Link
import org.matsim.api.core.v01.population.Person
import org.matsim.vehicles.Vehicle

class SOAgentReplanningHandler(
  val agentsUnderControl: Set[Id[Person]],
  val maxPathAssignments: Int,
  val minimumReplanningWaitTime: SimTime
) extends PersonEntersVehicleEventHandler
    with PersonLeavesVehicleEventHandler
    with PersonStuckEventHandler
    with LinkEnterEventHandler
    with LinkLeaveEventHandler
    with VehicleEntersTrafficEventHandler
    with VehicleLeavesTrafficEventHandler
    with LazyLogging {

  // this collection tracks which agents are currently active in the simulation,
  // along with some state data about them
  private val agentsInSimulation: collection.mutable.Map[Id[Person], AgentData]   = collection.mutable.Map.empty
  private val vehiclesForPersons: collection.mutable.Map[Id[Vehicle], Id[Person]] = collection.mutable.Map.empty

  // tracks a rolling average
  private var avgPathsAssigned: Double         = 0
  private var cntPathsAssigned: Int            = 0
  private var avgFailedRoutingAttempts: Double = 0
  private var sumFailedRoutingAttempts: Int    = 0
  private var cntFailedRoutingAttempts: Int    = 0
  def getAvgPathsAssigned: Double              = avgPathsAssigned
  def getAvgFailedRoutingAttempts: Double      = avgFailedRoutingAttempts
  def getSumFailedRoutingAttempts: Int         = sumFailedRoutingAttempts

  /**
    * recognizes agents with a System-Optimal routing agenda
    *
    * @param agent an agent in MATSim
    * @return whether agent is an SO agent
    */
  def isUnderControl(agent: Id[Person]): Boolean = agentsUnderControl(agent)

  /**
    * looks at all active SO agents and returns the ones that have not exceeded this.maxPathAssignments
    * and that have spent at least this.minimumReplanningWaitTime since they were last replanned
    *
    * @param currentSimTime the current sim time
    * @return list of agents eligible for replanning
    */
  def getActiveAndEligibleForReplanning(currentSimTime: SimTime): List[AgentData] = {
    agentsInSimulation
      .filter {
        case (_, agentData) =>
          val hasNotExceededMaxAssignments: Boolean =
            agentData.numberOfPathAssignments < this.maxPathAssignments
          val hasSurpassedMinReplanningWaitTime: Boolean =
            currentSimTime - agentData.mostRecentTimePlanned.getOrElse(SimTime.Zero) >= minimumReplanningWaitTime
          // removed - same as the "hasSurpassedMinReplanningWaitTime"
//          val hasSurpassedMinRequestUpdateThreshold: Boolean =
//            currentSimTime - agentData.mostRecentTimePlanned.getOrElse(SimTime.Zero) > minRequestUpdateThreshold
          hasNotExceededMaxAssignments && hasSurpassedMinReplanningWaitTime // && hasSurpassedMinRequestUpdateThreshold
      }
      .values
      .toList
  }

  /**
    * increments the count of path assignments and the stored most recent replanning time
    *
    * @param agent agent to update
    * @param currentSimTime current sim time
    * @return
    */
  def incrementAgentDataDueToReplanning(agent: Id[Person], currentSimTime: SimTime): Option[Unit] =
    agentsInSimulation
      .get(agent)
      .map { a => agentsInSimulation.update(agent, a.incrementPathAssignmentCount(currentSimTime)) }

  /**
    * a route plan failed; account for it
    * @param agent the agent we are adding the count to
    */
  def incrementNumberFailedRoutingAttempts(agent: Id[Person]): Unit = {
    agentsInSimulation
      .get(agent)
      .foreach { a => agentsInSimulation.update(agent, a.incrementNumberFailedRoutingAttempts()) }
  }

  /**
    * gets the count of replanning events that this agent has experienced in this iteration
    *
    * @param agent this agent
    * @return replanning count
    */
  def getReplanningCountForAgent(agent: Id[Person]): Option[Int] =
    agentsInSimulation.get(agent).map { _.numberOfPathAssignments }

  /**
    * gets the most recent SimTime that this agent was re-planned, unless they have not yet
    * been re-planned, in which case return None
    * @param agent this agent
    * @return an optional most recent time planned
    */
  def getMostRecentTimePlannedForAgent(agent: Id[Person]): Option[SimTime] =
    agentsInSimulation.get(agent).flatMap { _.mostRecentTimePlanned }

  /**
    * gets the departure time for this agent's current trip (that has just ended, so we can store it)
    *
    * @param agent the agent
    * @return departure time of current leg
    */
  def getDepartureTimeForAgent(agent: Id[Person]): Option[DepartureTime] =
    agentsInSimulation.get(agent).map { _.timeEnteredVehicle }

  /**
    * begin tracking an agent, allowing us to know how often we can replan them,
    * and collecting data on their route assignment history
    * @param event the "enters vehicle" event
    */
  def beginTrackingAgent(event: PersonEntersVehicleEvent): Unit = {
    val agentData: AgentData = AgentData(
      event.getPersonId,
      event.getVehicleId,
      DepartureTime(event.getTime.toInt)
    )
    agentsInSimulation += (event.getPersonId  -> agentData)
    vehiclesForPersons += (event.getVehicleId -> event.getPersonId)
  }

  /**
    * collect stats on agent and them remove them from tracking
    * @param personId person who just left their vehicle or got stuck
    */
  def stopTrackingAgent(personId: Id[Person]): Unit = {
    agentsInSimulation.get(personId) match {
      case None =>
        logger.warn(s"stop tracking agent called on person $personId who was not being tracked")
      case Some(agentData) =>
        // update our average paths assigned per SO agent
        avgPathsAssigned = SOAgentReplanningHandler.addToRollingAverage(
          observation = agentData.numberOfPathAssignments,
          rollingAverage = avgPathsAssigned,
          n = cntPathsAssigned
        )
        cntPathsAssigned += 1

        // update failed routing attempts
        avgFailedRoutingAttempts = SOAgentReplanningHandler.addToRollingAverage(
          observation = agentData.numberFailedRoutingAttempts,
          rollingAverage = avgFailedRoutingAttempts,
          n = cntFailedRoutingAttempts
        )
        cntFailedRoutingAttempts += 1
        sumFailedRoutingAttempts += agentData.numberFailedRoutingAttempts

        // remove agent from tracking
        agentsInSimulation -= personId
        vehiclesForPersons -= agentData.vehicleId
    }
  }

  /**
    * records the time we entered a link
    * @param personId the person to track
    * @param currentTime the current time
    */
  def trackEnteringALink(personId: Id[Person], currentTime: SimTime): Unit =
    if (agentsUnderControl(personId)) {
      agentsInSimulation.get(personId) match {
        case None =>
          logger.warn(s"personId $personId entering a link but not already tracked")
        case Some(agentData) =>
          val updatedAgentData: AgentData = agentData.copy(currentLinkEnterTime = Some { currentTime })
          this.agentsInSimulation.update(personId, updatedAgentData)
      }
    }

  /**
    * computes the duration of time we spent in a link and records it
    * @param personId the person to track
    * @param linkId the link they are exiting
    * @param currentTime the current time
    */
  def trackLeavingALink(personId: Id[Person], linkId: Id[Link], currentTime: SimTime): Unit =
    if (agentsUnderControl(personId)) {
      agentsInSimulation.get(personId) match {
        case None =>
          logger.warn(s"personId $personId leaving a link $linkId but agent was not being tracked")
        case Some(agentData) =>
          agentData.currentLinkEnterTime match {
            case None =>
              logger.warn(s"personId $personId leaving link $linkId but no record having entered it")
            case Some(currentLinkEnterTime) =>
              val linkTravelTime: SimTime         = currentTime - currentLinkEnterTime
              val LinkTraversal: (Id[Link], Cost) = (linkId, Cost(linkTravelTime.value))
              val updatedAgentData: AgentData =
                agentData.copy(
                  currentLinkEnterTime = None,
                  reverseExperiencedRoute = LinkTraversal +: agentData.reverseExperiencedRoute
                )
              this.agentsInSimulation.update(personId, updatedAgentData)
          }
      }
    }

  // for more information on these events, see The MATSim Book, page 19, Fig 2.2: Mobsim events
  // a traversal visits events in this order:
  //   Enters Vehicle
  //     Enters Traffic
  //     Leaves Link  <--\ loop
  //     Enters Link  ---/
  //     Leaves Traffic
  //   Leaves Vehicle

  /**
    * when a person enters a vehicle, begin tracking their replanning data
    * @param event person enters vehicle event
    */
  def handleEvent(event: PersonEntersVehicleEvent): Unit =
    if (agentsUnderControl(event.getPersonId)) this.beginTrackingAgent(event)

  /**
    * when a person leaves a vehicle, stop tracking their replanning data
    * @param event person leaves vehicle event
    */
  def handleEvent(event: PersonLeavesVehicleEvent): Unit =
    if (agentsUnderControl(event.getPersonId)) this.stopTrackingAgent(event.getPersonId)

  /**
    * handle stuck agents the same way we handle agents that leave (aka, still report their stats)
    * @param event person gets stuck (and is removed from simulation) event
    */
  def handleEvent(event: PersonStuckEvent): Unit =
    if (agentsUnderControl(event.getPersonId)) this.stopTrackingAgent(event.getPersonId)

  def handleEvent(event: LinkEnterEvent): Unit = {
    this.vehiclesForPersons.get(event.getVehicleId) match {
      case None =>
      // logger.debug(s"entering link with unregistered vehicle ${event.getVehicleId} presumed UE agent")
      case Some(personId) =>
        this.trackEnteringALink(personId, SimTime(event.getTime.toInt))
    }
  }

  def handleEvent(event: LinkLeaveEvent): Unit = {
    this.vehiclesForPersons.get(event.getVehicleId) match {
      case None =>
      // logger.debug(s"leaving link with unregistered vehicle ${event.getVehicleId} presumed UE agent")
      case Some(personId) =>
        this.trackLeavingALink(personId, event.getLinkId, SimTime(event.getTime.toInt))
    }
  }

  def handleEvent(event: VehicleEntersTrafficEvent): Unit =
    this.trackEnteringALink(event.getPersonId, SimTime(event.getTime.toInt))

  def handleEvent(event: VehicleLeavesTrafficEvent): Unit =
    this.trackLeavingALink(event.getPersonId, event.getLinkId, SimTime(event.getTime.toInt))

  /**
    * restart between iterations
    */
  def clear(): Unit = {
    agentsInSimulation.clear()
    avgPathsAssigned = 0.0
    cntPathsAssigned = 0
    avgFailedRoutingAttempts = 0.0
    cntFailedRoutingAttempts = 0
    sumFailedRoutingAttempts = 0
  }
}

object SOAgentReplanningHandler {

  /**
    * helps compute a rolling average
    * @param observation the n+1th value to add to the rolling average
    * @param rollingAverage the average over the n values preceeding
    * @param n number of observations in the current rolling average
    * @return updated rolling average
    */
  def addToRollingAverage(observation: Double, rollingAverage: Double, n: Int): Double = {
    rollingAverage + (observation - rollingAverage) / (n + 1)
  }
}

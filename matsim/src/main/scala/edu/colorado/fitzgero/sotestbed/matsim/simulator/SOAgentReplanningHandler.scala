package edu.colorado.fitzgero.sotestbed.matsim.simulator

import edu.colorado.fitzgero.sotestbed.matsim.simulator.SOAgentReplanningHandler.AgentData
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.events.handler.{PersonEntersVehicleEventHandler, PersonLeavesVehicleEventHandler, PersonStuckEventHandler}
import org.matsim.api.core.v01.events.{PersonEntersVehicleEvent, PersonLeavesVehicleEvent, PersonStuckEvent}
import org.matsim.api.core.v01.population.Person

class SOAgentReplanningHandler(
  val agentsUnderControl: Set[Id[Person]],
  val requestUpdateCycle: SimTime,
  val maxPathAssignments: Int,
  val minimumReplanningWaitTime: SimTime
) extends PersonEntersVehicleEventHandler
    with PersonLeavesVehicleEventHandler
    with PersonStuckEventHandler {

  // this collection tracks which agents are currently active in the simulation,
  // along with some state data about them
  private val agentsInSimulation: collection.mutable.Map[Id[Person], AgentData] = collection.mutable.Map.empty

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
    * adhere to the user-specified request update schedule
    * @param currentSimTime the current sim time
    * @return whether to update the requests at this sim time
    */
  def updateRequestsThisSimTime(currentSimTime: SimTime): Boolean = currentSimTime % requestUpdateCycle == SimTime.Zero

  /**
    * looks at all active SO agents and returns the ones that have not exceeded this.maxPathAssignments
    * and that have spent at least this.minimumReplanningWaitTime since they were last replanned
    *
    * @param currentSimTime the current sim time
    * @return list of agents eligible for replanning
    */
  def getActiveAndEligibleForReplanning(currentSimTime: SimTime): List[Id[Person]] = {
    if (currentSimTime % requestUpdateCycle != SimTime.Zero) {
      // nothing to report this round
      List.empty
    } else {
      agentsInSimulation
        .filter {
          case (_, agentData) =>
            val hasNotExceededMaxAssignments: Boolean =
              agentData.numberOfPathAssignments < this.maxPathAssignments
            val hasSurpassedMinReplanningWaitTime: Boolean =
              currentSimTime - agentData.mostRecentTimePlanned.getOrElse(SimTime.Zero) > minimumReplanningWaitTime
            hasNotExceededMaxAssignments && hasSurpassedMinReplanningWaitTime
        }
        .keys
        .toList
    }
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
      .map { a =>
        agentsInSimulation.update(agent, a.incrementPathAssignmentCount(currentSimTime))
      }

  /**
    * a route plan failed; account for it
    * @param agent the agent we are adding the count to
    */
  def incrementNumberFailedRoutingAttempts(agent: Id[Person]): Unit = {
    agentsInSimulation
      .get(agent)
      .foreach { a =>
        agentsInSimulation.update(agent, a.incrementNumberFailedRoutingAttempts())
      }
  }

  /**
    * gets the count of replanning events that this agent has experienced in this iteration
    *
    * @param agent this agent
    * @return replanning count
    */
  def getReplanningCountForAgent(agent: Id[Person]): Option[Int] =
    for {
      agentData <- agentsInSimulation.get(agent)
    } yield agentData.numberOfPathAssignments

  /**
    * gets the most recent SimTime that this agent was re-planned, unless they have not yet
    * been re-planned, in which case return None
    * @param agent this agent
    * @return an optional most recent time planned
    */
  def getMostRecentTimePlannedForAgent(agent: Id[Person]): Option[SimTime] =
    for {
      agentData             <- agentsInSimulation.get(agent)
      mostRecentTimePlanned <- agentData.mostRecentTimePlanned
    } yield mostRecentTimePlanned

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
    * @param agent the agent to track
    * @param time the time the agent entered their vehicle
    */
  def beginTrackingAgent(agent: Id[Person], time: Double): Unit =
    agentsInSimulation += (agent -> AgentData(agent, DepartureTime(time.toInt)))

  /**
    * collect stats on agent and them remove them from tracking
    * @param agent the agent to stop tracking
    */
  def stopTrackingAgent(agent: Id[Person]): Unit = {
    agentsInSimulation.get(agent) match {
      case None            =>
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
        agentsInSimulation -= agent
    }
  }

  // for more information on these events, see The MATSim Book, page 19, Fig 2.2: Mobsim events

  /**
    * when a person enters a vehicle, begin tracking their replanning data
    * @param event person enters vehicle event
    */
  def handleEvent(event: PersonEntersVehicleEvent): Unit =
    if (agentsUnderControl(event.getPersonId)) this.beginTrackingAgent(event.getPersonId, event.getTime)

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
    * data class containing information about this agent's current trip
    * @param personId the agent
    * @param timeEnteredVehicle sim time they entered their vehicle
    * @param numberOfPathAssignments number of times they have been assigned a route from our algorithm
    * @param numberFailedRoutingAttempts number of times that assignment was rejected/failed
    * @param mostRecentTimePlanned the most recent time the agent accepted a new route
    */
  private final case class AgentData(
    personId: Id[Person],
    timeEnteredVehicle: DepartureTime,
    numberOfPathAssignments: Int = 0,
    numberFailedRoutingAttempts: Int = 0,
    mostRecentTimePlanned: Option[SimTime] = None,
  ) {

    def incrementPathAssignmentCount(simTime: SimTime): AgentData =
      this.copy(
        numberOfPathAssignments = this.numberOfPathAssignments + 1,
        mostRecentTimePlanned = Some { simTime }
      )

    def incrementNumberFailedRoutingAttempts(): AgentData =
      this.copy(
        numberFailedRoutingAttempts = this.numberFailedRoutingAttempts + 1
      )
  }

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

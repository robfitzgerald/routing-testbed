package edu.colorado.fitzgero.sotestbed.matsim

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.events.handler.{PersonEntersVehicleEventHandler, PersonLeavesVehicleEventHandler}
import org.matsim.api.core.v01.events.{PersonEntersVehicleEvent, PersonLeavesVehicleEvent}
import org.matsim.api.core.v01.population.Person
import org.matsim.vehicles.Vehicle

class AgentsInSimulationNeedingReplanningHandler(
  val agentsUnderControl: Set[Id[Person]],
  val requestUpdateCycle: SimTime,
  val maxPathAssignments: Int,
) extends PersonEntersVehicleEventHandler
    with PersonLeavesVehicleEventHandler {

  import edu.colorado.fitzgero.sotestbed.matsim.AgentsInSimulationNeedingReplanningHandler.AgentData

  // this collection tracks which agents are currently active in the simulation,
  // along with some state data about them
  private val agentsInSimulation: collection.mutable.Map[Id[Person], AgentData] = collection.mutable.Map.empty

//  // this collection associates vehicles with persons
//  private val vehiclesForAgents: collection.mutable.Map[Id[Vehicle], Id[Person]] = collection.mutable.Map.empty

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
        .filter{ case (_, agentData) => agentData.numberOfPathAssignments < this.maxPathAssignments}
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
    * gets the count of replanning events that this agent has experienced in this iteration
    *
    * @param agent this agent
    * @return replanning count
    */
  def getReplanningCountForAgent(agent: Id[Person]): Option[Int] = agentsInSimulation.get(agent).map { _.numberOfPathAssignments }

  /**
    * gets the departure time for this agent's current trip (that has just ended, so we can store it)
    *
    * @param agent the agent
    * @return departure time of current leg
    */
  def getDepartureTimeForAgent(agent: Id[Person]): Option[MATSimProxy.DepartureTime] =
    agentsInSimulation.get(agent).map { _.departureTimeOfCurrentLeg }

  // for more information on these events, see The MATSim Book, page 19, Fig 2.2: Mobsim events

//  /**
//    * start tracking agents who have departed from their previous activity (before entering their vehicle)
//    *
//    * @param event the event where an agent departs and heads to the road network
//    */
//  def handleEvent(event: PersonDepartureEvent): Unit =
//    if (agentsUnderControl(event.getPersonId)) {
//      agentsInSimulation += (event.getPersonId -> AgentData(MATSimProxy.DepartureTime(event.getTime.toInt)))
//    }
//
//  /**
//    * stop tracking agents who have arrived at their activity (after leaving their vehicle)
//    *
//    * @param event the event where an agent arrives at their activity
//    */
//  def handleEvent(event: PersonArrivalEvent): Unit =
//    if (agentsUnderControl(event.getPersonId)) {
//      agentsInSimulation -= event.getPersonId
//    }
//
//

  /**
    * when a person enters a vehicle, begin tracking their replanning data
    * @param event person enters vehicle event
    */
  def handleEvent(event: PersonEntersVehicleEvent): Unit = {
      if (agentsUnderControl(event.getPersonId)) {
        // begin tracking agent data in simulation
        agentsInSimulation +=
          (event.getPersonId ->
            AgentData(
              event.getPersonId,
              event.getVehicleId,
              MATSimProxy.DepartureTime(event.getTime.toInt),
            )
          )
        // provide agent lookup by vehicle id
//        vehiclesForAgents += (event.getVehicleId -> event.getPersonId)
      }
  }

  /**
    * when a person leaves a vehicle, stop tracking their replanning data
    * @param event person leaves vehicle event
    */
  def handleEvent(event: PersonLeavesVehicleEvent): Unit = {
    if (agentsUnderControl(event.getPersonId)) {
      agentsInSimulation -= event.getPersonId
//      vehiclesForAgents -= event.getVehicleId
    }
  }

//  /**
//    * when a person enters a link, update this person's current link location
//    * @param event vehicle enters link event
//    */
//  def handleEvent(event: LinkEnterEvent): Unit = {
//    if (vehiclesForAgents.isDefinedAt(event.getVehicleId)) {
//      for {
//        personId <- vehiclesForAgents.get(event.getVehicleId)
//        agentData <- agentsInSimulation.get(personId)
//      } {
//        // update AgentData with new current link
////        val updatedAgentData = agentData.copy(currentLink = Some{ event.getLinkId })
////        agentsInSimulation.update(personId, updatedAgentData)
//      }
//    }
//  }


  /**
    * restart between iterations
    */
  def clear(): Unit = {
    agentsInSimulation.clear()
  }
}

object AgentsInSimulationNeedingReplanningHandler {
  private final case class AgentData(
    personId: Id[Person],
    vehicleId: Id[Vehicle],
    departureTimeOfCurrentLeg: MATSimProxy.DepartureTime,
    numberOfPathAssignments: Int = 0,
    mostRecentTimePlanned: Option[SimTime] = None,
//    currentLink: Option[Id[Link]] = None,
  ) {

    def incrementPathAssignmentCount(simTime: SimTime): AgentData =
      this.copy(
        numberOfPathAssignments = this.numberOfPathAssignments + 1,
        mostRecentTimePlanned = Some { simTime }
      )

  }
}

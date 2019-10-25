package edu.colorado.fitzgero.sotestbed.matsim


import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.events.handler.{PersonArrivalEventHandler, PersonDepartureEventHandler}
import org.matsim.api.core.v01.events.{Event, PersonArrivalEvent, PersonDepartureEvent}
import org.matsim.api.core.v01.network.Link
import org.matsim.api.core.v01.population.Person

class AgentsInSimulationNeedingReplanningHandler(
  agentsUnderControl: Set[Id[Person]]
) extends PersonArrivalEventHandler with PersonDepartureEventHandler {

  import edu.colorado.fitzgero.sotestbed.matsim.AgentsInSimulationNeedingReplanningHandler.AgentData

  private val agentsInSimulation: collection.mutable.Map[Id[Person], AgentData] = collection.mutable.Map.empty

  def getActiveAgentIds: List[Id[Person]] = agentsInSimulation.keys.toList

  def getPathForAgent(agent: Id[Person]): Option[List[Id[Link]]] = agentsInSimulation.get(agent).map{_.path}

  def getNumberOfPathAssignmentsForAgent(agent: Id[Person]): Option[Int] = agentsInSimulation.get(agent).map{_.numberOfPathAssignments}

//  def getRequestDataForAgent: Unit = ???
//
//  def getRequestsUnderControl: Unit = ???

  def addPathToAgent(agent: Id[Person], path: List[Id[Link]]): Unit = {
    agentsInSimulation.get(agent) match {
      case None => () // todo: should log here
      case Some(agentData) =>
        agentsInSimulation.update(
          agent,
          agentData.addPath(path)
        )
    }
  }

  // for more information on these events, see The MATSim Book, page 19, Fig 2.2: Mobsim events

  /**
    * start tracking agents who have departed from their previous activity
    * @param event the event where an agent departs and heads to the road network
    */
  def handleEvent(event: PersonDepartureEvent): Unit =
    if (agentsUnderControl(event.getPersonId)) {
      agentsInSimulation + (event.getPersonId -> AgentData())
    }

  /**
    * stop tracking agents who have arrived at their activity
    * @param event the event where an agent arrives at their activity
    */
  def handleEvent(event: PersonArrivalEvent): Unit =
    if (agentsUnderControl(event.getPersonId)) agentsInSimulation - event.getPersonId

  /**
    * restart between iterations
    */
  def clear(): Unit = {
    agentsInSimulation.clear()
  }
}

object AgentsInSimulationNeedingReplanningHandler {
  private final case class AgentData(
      path: List[Id[Link]] = List.empty,
      numberOfPathAssignments: Int = 0
  ) {
    def addPath(path: List[Id[Link]]): AgentData =
      this.copy(
        path = path,
        numberOfPathAssignments = numberOfPathAssignments + 1
      )
  }
}
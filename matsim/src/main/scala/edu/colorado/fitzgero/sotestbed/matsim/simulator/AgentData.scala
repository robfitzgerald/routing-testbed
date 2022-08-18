package edu.colorado.fitzgero.sotestbed.matsim.simulator

import org.matsim.api.core.v01.Id
import edu.colorado.fitzgero.sotestbed.model.numeric._
import org.matsim.api.core.v01.population.Person
import org.matsim.vehicles.Vehicle
import org.matsim.api.core.v01.network.Link
import org.matsim.core.mobsim.qsim.QSim
import scala.jdk.CollectionConverters._
import org.matsim.core.mobsim.framework.MobsimAgent

/**
  * data class containing information about this agent's current trip
  * @param personId the agent
  * @param timeEnteredVehicle sim time they entered their vehicle
  * @param numberOfPathAssignments number of times they have been assigned a route from our algorithm
  * @param numberFailedRoutingAttempts number of times that assignment was rejected/failed
  * @param mostRecentTimePlanned the most recent time the agent accepted a new route
  * @param currentLinkEnterTime if we have entered a link, we store the time we entered it
  * @param reverseExperiencedRoute a reverse-order stored list of visited link data (due to fast List prepend op)
  */
final case class AgentData(
  personId: Id[Person],
  vehicleId: Id[Vehicle],
  timeEnteredVehicle: DepartureTime,
  numberOfPathAssignments: Int = 0,
  numberFailedRoutingAttempts: Int = 0,
  mostRecentTimePlanned: Option[SimTime] = None,
  currentLinkEnterTime: Option[SimTime] = None,
  reverseExperiencedRoute: List[(Id[Link], Cost)] = List.empty
) {

  /**
    * @return the experienced route in the correct order
    */
  def getExperiencedRoute: List[(Id[Link], Cost)] = this.reverseExperiencedRoute.reverse

  def incrementPathAssignmentCount(simTime: SimTime): AgentData =
    this.copy(
      numberOfPathAssignments = this.numberOfPathAssignments + 1,
      mostRecentTimePlanned = Some { simTime }
    )

  def incrementNumberFailedRoutingAttempts(): AgentData =
    this.copy(
      numberFailedRoutingAttempts = this.numberFailedRoutingAttempts + 1
    )

  /**
    * pulls out the current state of this driver out of the queue simulation
    *
    * @param qSim the queue simulation state
    */
  def extractAgentStateOrError(qSim: QSim): Either[Error, AgentState] =
    AgentState.forAgent(qSim, this.personId, this.vehicleId)

}

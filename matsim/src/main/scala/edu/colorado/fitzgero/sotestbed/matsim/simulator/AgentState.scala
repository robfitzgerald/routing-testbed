package edu.colorado.fitzgero.sotestbed.matsim.simulator

import org.matsim.core.mobsim.framework.MobsimAgent
import org.matsim.api.core.v01.population.Person
import org.matsim.vehicles.Vehicle
import org.matsim.api.core.v01.population.Leg
import org.matsim.core.mobsim.qsim.agents.WithinDayAgentUtils
import org.matsim.core.mobsim.qsim.QSim
import org.matsim.api.core.v01.Id
import scala.jdk.CollectionConverters._

final case class AgentState(
  mobsimAgent: MobsimAgent,
  person: Person,
  vehicle: Vehicle
) {

  def getModifiableLeg: Either[Error, Leg] = {
    Option(WithinDayAgentUtils.getModifiableCurrentLeg(mobsimAgent)) match {
      case None =>
        Left(new Error(s"attempting to get current trip leg for ${person.getId} that doesn't exist"))
      case Some(leg) => Right(leg)
    }
  }
}

object AgentState {

  def forAgent(qSim: QSim, personId: Id[Person], vehicleId: Id[Vehicle]): Either[Error, AgentState] = {
    val result = for {
      mobsimAgent <- qSim.getAgents.asScala.toMap.get(personId)
      person      <- qSim.getScenario.getPopulation.getPersons.asScala.get(personId)
      vehicle     <- qSim.getVehicles.asScala.get(vehicleId).map { _.getVehicle }
    } yield AgentState(mobsimAgent, person, vehicle)
    result match {
      case None        => Left(new Error(s"agent $personId not found in QSim"))
      case Some(value) => Right(value)
    }
  }
}

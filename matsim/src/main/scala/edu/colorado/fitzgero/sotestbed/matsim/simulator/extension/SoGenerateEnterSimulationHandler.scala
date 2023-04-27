package edu.colorado.fitzgero.sotestbed.matsim.simulator.extension

import org.matsim.api.core.v01.events.handler.VehicleEntersTrafficEventHandler
import edu.colorado.fitzgero.sotestbed.matsim.simulator.SOAgentReplanningHandler
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.population.Person
import org.matsim.vehicles.Vehicle
import org.matsim.api.core.v01.events.VehicleEntersTrafficEvent
import edu.colorado.fitzgero.sotestbed.matsim.simulator.DepartureTime
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import com.typesafe.scalalogging.LazyLogging
import org.matsim.api.core.v01.network.Link
import edu.colorado.fitzgero.sotestbed.matsim.simulator.GenerateAgentData
import org.matsim.core.trafficmonitoring.TravelTimeCalculator
import org.matsim.core.mobsim.qsim.QSim
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData
import cats.effect.unsafe.implicits.global

class SoGenerateEnterSimulationHandler(
  soReplanningThisIteration: Boolean,
  qSim: QSim,
  travelTimeLookupFunction: Id[Link] => SimTime,
  completePathStore: collection.mutable.Map[Id[Person], Map[DepartureTime, List[Id[Link]]]],
  newSOAgentBatchData: collection.mutable.ListBuffer[AgentBatchData]
) extends VehicleEntersTrafficEventHandler
    with LazyLogging {

  override def handleEvent(event: VehicleEntersTrafficEvent): Unit = {
    if (soReplanningThisIteration) {
      val personId      = event.getPersonId
      val vehicleId     = event.getVehicleId
      val departureTime = DepartureTime(event.getTime.toInt)
      val simTime       = SimTime(event.getTime.toInt)

      // during so-replanning iterations, they are implicitly forced to apply their routes
      // when not receiving so-replanning routing, the SO agent routes would by default
      // be assigned by MATSim using the built-in GA policy.
      // noop

      // wipe the stored routes, they will be over-written
      completePathStore.remove(personId)

      // let the routing algorithm know this agent has entered the system
      val generateArgs = GenerateAgentData.GenerateEnterSimulation(
        travelTimeLookupFunction,
        personId,
        vehicleId,
        simTime
      )
      val msg = GenerateAgentData
        .generateEnterSimulation(qSim, generateArgs)
        .unsafeRunSync
      newSOAgentBatchData.prepend(msg)
    }
  }
}

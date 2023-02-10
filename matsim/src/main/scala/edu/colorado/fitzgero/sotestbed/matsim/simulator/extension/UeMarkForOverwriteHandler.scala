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

class UeMarkForOverwriteHandler(
  soAgentReplanningHandler: SOAgentReplanningHandler,
  departureTimeStore: collection.mutable.Map[Id[Person], DepartureTime],
  markForUEPathOverwrite: collection.mutable.Map[Id[Vehicle], (Id[Person], SimTime)],
  ueAgentAssignedDijkstraRoute: collection.mutable.Set[Id[Person]],
  useExternalRoutingEngineForSelfishAgents: Boolean
) extends VehicleEntersTrafficEventHandler
    with LazyLogging {

  override def handleEvent(event: VehicleEntersTrafficEvent): Unit = {
    val personId      = event.getPersonId
    val vehicleId     = event.getVehicleId
    val departureTime = DepartureTime(event.getTime.toInt)
    val simTime       = SimTime(event.getTime.toInt)

    departureTimeStore.update(personId, departureTime)

    if (useExternalRoutingEngineForSelfishAgents &&
        !soAgentReplanningHandler.isUnderControl(personId)) {
      // this is a UE agent who needs selfish routing. we will
      // mark them to have dijkstra routing when they enter their
      // first path edge

      markForUEPathOverwrite.update(vehicleId, (personId, simTime))
      ueAgentAssignedDijkstraRoute -= personId
      logger.debug(s"agent $personId marked for routing")
    }
  }

}

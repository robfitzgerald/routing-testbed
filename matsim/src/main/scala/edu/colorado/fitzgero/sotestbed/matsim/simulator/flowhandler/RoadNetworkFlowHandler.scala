package edu.colorado.fitzgero.sotestbed.matsim.simulator.flowhandler

import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.events.handler.{LinkEnterEventHandler, LinkLeaveEventHandler, PersonStuckEventHandler}
import org.matsim.api.core.v01.events.{LinkEnterEvent, LinkLeaveEvent, PersonStuckEvent}
import org.matsim.api.core.v01.network.Link
import org.matsim.api.core.v01.events.handler.VehicleAbortsEventHandler
import org.matsim.api.core.v01.events.VehicleAbortsEvent
import org.matsim.api.core.v01.events.handler.VehicleEntersTrafficEventHandler
import org.matsim.api.core.v01.events.handler.VehicleLeavesTrafficEventHandler
import org.matsim.api.core.v01.events.VehicleLeavesTrafficEvent
import org.matsim.api.core.v01.events.VehicleEntersTrafficEvent
import com.typesafe.scalalogging.LazyLogging
import org.matsim.vehicles.Vehicle
import org.matsim.core.mobsim.qsim.QSim
import scala.collection.JavaConverters._

/**
  * keeps track of changes in the road network, based on events where vehicles enter and leave the network,
  * for an absolute representation of flow values. only stores entries for links with non-zero flows.
  */
class RoadNetworkFlowHandler
    extends LinkEnterEventHandler
    with LinkLeaveEventHandler
    with VehicleEntersTrafficEventHandler
    with VehicleLeavesTrafficEventHandler
    // with PersonStuckEventHandler
    with VehicleAbortsEventHandler
    with LazyLogging {

  private val linkRecords: collection.mutable.Map[Id[Link], RoadNetworkFlowRecord] = collection.mutable.Map.empty

  def clear(): Unit = {
    if (linkRecords.nonEmpty) {
      // no vehicles should be on the network when clear() is called, so, this means
      // we are not correctly managing our state updates from the event loggers
      val count = linkRecords.size
      val topTenCounts = linkRecords.toList
        .sortBy { case (_, rec) => -rec.getFlowCount }
        .take(10)
        .map { case (id, cnt) => f"$id: $cnt" }
        .mkString("{", ", ", "}")
      logger.warn(f"ending iteration but found $count links still had flows recorded; top 10:")
      logger.warn(topTenCounts)
    }

    this.linkRecords.clear()
  }

  def getCount(linkId: Id[Link]): Int = this.linkRecords.get(linkId).map(_.getFlowCount).getOrElse(0)

  def getFlow(linkId: Id[Link]): Option[Flow] =
    this.linkRecords.get(linkId).map(o => Flow(o.getFlowCount.toDouble))

  /**
    * builds a travel time lookup table that, for a given link, checks if we
    * have observed traversals since the $binStartTime. if not, the table
    * looks up the free flow travel time for the link instead as a fallback
    * (based on the assumption that there are no agents competing for right of way)
    *
    * @param binStartTime start time for collecting travel time observations. collects
    *                     all completed trips from this start time to current
    * @return either observed travel time average or the free flow travel time
    */
  def buildTravelTimeEstimator(binStartTime: SimTime): QSim => Id[Link] => SimTime = {
    val obsTable = updateAndCollectLinkObservations(binStartTime)
    (qSim: QSim) =>
      val linkTable = qSim.getNetsimNetwork.getNetwork.getLinks.asScala
      (linkId: Id[Link]) =>
        val observedTravelTimeResult = for {
          obs <- obsTable.get(linkId)
          tt  <- obs.averageTraversalDurationSeconds
        } yield SimTime(math.max(1.0, tt))

        observedTravelTimeResult.getOrElse {
          linkTable.get(linkId) match {
            case None =>
              throw new Exception(f"internal error - missing link $linkId from QSim")
            case Some(link) =>
              SimTime(math.max(1.0, link.getLength / link.getFreespeed))
          }
        }
  }

  def updateAndCollectLinkObservations(binStartTime: SimTime): Map[Id[Link], RoadNetworkFlowObservation] = {
    val observations = for {
      (linkId, record) <- this.linkRecords.toMap
      (updatedRecord, obs) = record.updateAndCollect(binStartTime)
    } yield {
      linkRecords.update(linkId, updatedRecord) // side effect
      (linkId, obs)
    }
    observations
  }

  def processEnterLink(linkId: Id[Link], vehicleId: Id[Vehicle], time: Double): Unit = {
    val record  = this.linkRecords.getOrElse(linkId, RoadNetworkFlowRecord())
    val updated = record.processLinkEnter(vehicleId, SimTime(time))
    this.linkRecords.update(linkId, updated)
  }

  def processExitLink(linkId: Id[Link], vehicleId: Id[Vehicle], time: Double): Unit = {
    val record  = this.linkRecords.getOrElse(linkId, RoadNetworkFlowRecord())
    val updated = record.processLinkExit(vehicleId, SimTime(time))
    this.linkRecords.update(linkId, updated)
  }

  // def incrementCount(linkId: Id[Link]): Unit = this.linkRecords.update(linkId, getCount(linkId) + 1)

  // def decrementCount(linkId: Id[Link]): Unit = {
  //   val prevCount = getCount(linkId)
  //   // somehow we sometimes see decrementCount called on a linkId without
  //   // observed flows, which is maybe a sync issue with MATSim, or, maybe
  //   // we're not hooking on all the right handlers.
  //   if (prevCount == 0) throw new IllegalStateException(f"removing from link $linkId with count of 0")
  //   else if (prevCount == 1) this.linkRecords.remove(linkId)
  //   else this.linkRecords.update(linkId, prevCount - 1)
  // }

  def handleEvent(event: LinkEnterEvent): Unit =
    this.processEnterLink(event.getLinkId, event.getVehicleId, event.getTime)

  def handleEvent(event: LinkLeaveEvent): Unit =
    this.processExitLink(event.getLinkId, event.getVehicleId, event.getTime)

  def handleEvent(event: VehicleEntersTrafficEvent): Unit =
    this.processEnterLink(event.getLinkId, event.getVehicleId, event.getTime)

  def handleEvent(event: VehicleLeavesTrafficEvent): Unit =
    this.processExitLink(event.getLinkId, event.getVehicleId, event.getTime)

  // def handleEvent(event: PersonStuckEvent): Unit =
  // this.processExitLink(event.getLinkId, event.getVehicleId, event.getTime)

  def handleEvent(event: VehicleAbortsEvent): Unit =
    this.processExitLink(event.getLinkId, event.getVehicleId, event.getTime)
}

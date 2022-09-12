package edu.colorado.fitzgero.sotestbed.matsim.simulator

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

  private val linkFlows: collection.mutable.Map[Id[Link], Int] = collection.mutable.Map.empty

  def clear(): Unit = {
    if (linkFlows.nonEmpty) {
      // no vehicles should be on the network when clear() is called, so, this means
      // we are not correctly managing our state updates from the event loggers
      val count = linkFlows.size
      val topTenCounts = linkFlows.toList
        .sortBy { case (_, cnt) => -cnt }
        .take(10)
        .map { case (id, cnt) => f"$id: $cnt" }
        .mkString("{", ", ", "}")
      logger.warn(f"ending iteration but found $count links still had flows recorded; top 10:")
      logger.warn(topTenCounts)
    }

    this.linkFlows.clear()
  }

  def getCount(linkId: Id[Link]): Int = this.linkFlows.getOrElse(linkId, 0)

  def getFlow(linkId: Id[Link]): Option[Flow] =
    this.linkFlows.get(linkId).map { cnt => Flow(cnt.toDouble) }

  def incrementCount(linkId: Id[Link]): Unit = this.linkFlows.update(linkId, getCount(linkId) + 1)

  def decrementCount(linkId: Id[Link]): Unit = {
    val prevCount = getCount(linkId)
    // somehow we sometimes see decrementCount called on a linkId without
    // observed flows, which is maybe a sync issue with MATSim, or, maybe
    // we're not hooking on all the right handlers.
    if (prevCount == 0) throw new IllegalStateException(f"removing from link $linkId with count of 0")
    else if (prevCount == 1) this.linkFlows.remove(linkId)
    else this.linkFlows.update(linkId, prevCount - 1)
  }

  def handleEvent(event: LinkEnterEvent): Unit = incrementCount(event.getLinkId)

  def handleEvent(event: LinkLeaveEvent): Unit = decrementCount(event.getLinkId)

  def handleEvent(event: VehicleEntersTrafficEvent): Unit = incrementCount(event.getLinkId)

  def handleEvent(event: VehicleLeavesTrafficEvent): Unit = decrementCount(event.getLinkId)

  // def handleEvent(event: PersonStuckEvent): Unit = decrementCount(event.getLinkId)

  def handleEvent(event: VehicleAbortsEvent): Unit = decrementCount(event.getLinkId)
}

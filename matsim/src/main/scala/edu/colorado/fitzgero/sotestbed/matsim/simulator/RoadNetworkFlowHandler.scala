package edu.colorado.fitzgero.sotestbed.matsim.simulator

import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.events.handler.{LinkEnterEventHandler, LinkLeaveEventHandler, PersonStuckEventHandler}
import org.matsim.api.core.v01.events.{LinkEnterEvent, LinkLeaveEvent, PersonStuckEvent}
import org.matsim.api.core.v01.network.Link

/**
  * keeps track of changes in the road network, based on events where vehicles enter and leave the network,
  * for an absolute representation of flow values. only stores entries for links with non-zero flows.
  */
class RoadNetworkFlowHandler extends LinkEnterEventHandler with LinkLeaveEventHandler with PersonStuckEventHandler {

  private val linkFlows: collection.mutable.Map[Id[Link], Int] = collection.mutable.Map.empty

  def clear(): Unit = this.linkFlows.clear()

  def getCount(linkId: Id[Link]): Int = this.linkFlows.getOrElse(linkId, 0)

  def getFlow(linkId: Id[Link]): Option[Flow] =
    this.linkFlows.get(linkId).map { cnt => Flow(cnt.toDouble) }

  def incrementCount(linkId: Id[Link]): Unit = this.linkFlows.update(linkId, getCount(linkId) + 1)

  def decrementCount(linkId: Id[Link]): Unit = {
    val prevCount = getCount(linkId)
    if (prevCount == 0) throw new IllegalStateException(s"decrementing flow count of 0 for link $linkId")
    else if (prevCount == 1) this.linkFlows - linkId
    else this.linkFlows.update(linkId, prevCount - 1)
  }

  /**
    * increase count of a link due to a vehicle entering a link
    * @param event a matsim LinkEnterEvent
    */
  def handleEvent(event: LinkEnterEvent): Unit = incrementCount(event.getLinkId)

  /**
    * reduce count of a link due to a vehicle normally exiting a link
    * @param event
    */
  def handleEvent(event: LinkLeaveEvent): Unit = decrementCount(event.getLinkId)

  /**
    * remove a link flow count associated with a stuck person event
    * @param event
    */
  def handleEvent(event: PersonStuckEvent): Unit = decrementCount(event.getLinkId)
}

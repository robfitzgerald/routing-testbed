package edu.colorado.fitzgero.sotestbed.matsim.simulator

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.events.handler.{LinkEnterEventHandler, LinkLeaveEventHandler, PersonStuckEventHandler}
import org.matsim.api.core.v01.events.{LinkEnterEvent, LinkLeaveEvent, PersonStuckEvent}
import org.matsim.api.core.v01.network.Link

/**
  * keeps track of changes in the road network, based on events where vehicles enter and leave the network
  */
class RoadNetworkDeltaHandler extends LinkEnterEventHandler with LinkLeaveEventHandler with PersonStuckEventHandler {

  private val linkDeltas: collection.mutable.Map[Id[Link], Int] = collection.mutable.Map.empty

  def clear(): Unit = linkDeltas.clear()

  def getDeltas: Map[Id[Link], Int] = linkDeltas.toMap
  def getDeltasAsEdgeIds: Map[EdgeId, Int] = linkDeltas.toMap.map{ case (k, v) => EdgeId(k.toString) -> v }

  /**
    * increase count of a link due to a vehicle entering a link
    * @param event
    */
  def handleEvent(event: LinkEnterEvent): Unit =
    linkDeltas.get(event.getLinkId) match {
      case None => linkDeltas.update(event.getLinkId, 1)
      case Some(linkIdData) => linkDeltas.update(event.getLinkId, linkIdData + 1)
    }

  /**
    * reduce count of a link due to a vehicle normally exiting a link
    * @param event
    */
  def handleEvent(event: LinkLeaveEvent): Unit =
    linkDeltas.get(event.getLinkId) match {
      case None => linkDeltas.update(event.getLinkId, -1)
      case Some(linkIdData) => linkDeltas.update(event.getLinkId, linkIdData - 1)
    }

  /**
    * remove a link flow count associated with a stuck person event
    * @param event
    */
  def handleEvent(event: PersonStuckEvent): Unit = {
    linkDeltas.get(event.getLinkId) match {
      case None => linkDeltas.update(event.getLinkId, -1)
      case Some(linkIdData) => linkDeltas.update(event.getLinkId, linkIdData - 1)
    }
  }
}

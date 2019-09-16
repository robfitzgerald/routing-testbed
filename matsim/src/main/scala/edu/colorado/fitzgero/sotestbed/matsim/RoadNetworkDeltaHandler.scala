package edu.colorado.fitzgero.sotestbed.matsim

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.events.{LinkEnterEvent, LinkLeaveEvent}
import org.matsim.api.core.v01.events.handler.{LinkEnterEventHandler, LinkLeaveEventHandler}
import org.matsim.api.core.v01.network.Link

class RoadNetworkDeltaHandler extends LinkEnterEventHandler with LinkLeaveEventHandler {

  private val linkDeltas: collection.mutable.Map[Id[Link], Int] = collection.mutable.Map.empty

  def clear(): Unit = linkDeltas.clear()

  def getDeltas: Map[Id[Link], Int] = linkDeltas.toMap
  def getDeltasAsEdgeIds: Map[EdgeId, Int] = linkDeltas.toMap.map{ case (k, v) => EdgeId(k.toString) -> v }

  def handleEvent(event: LinkEnterEvent): Unit =
    linkDeltas.get(event.getLinkId) match {
      case None => linkDeltas.update(event.getLinkId, 1)
      case Some(linkIdData) => linkDeltas.update(event.getLinkId, linkIdData + 1)
    }

  def handleEvent(event: LinkLeaveEvent): Unit =
    linkDeltas.get(event.getLinkId) match {
      case None => linkDeltas.update(event.getLinkId, -1)
      case Some(linkIdData) => linkDeltas.update(event.getLinkId, linkIdData - 1)
    }
}

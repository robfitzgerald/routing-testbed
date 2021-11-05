package edu.colorado.fitzgero.sotestbed.matsim.simulator

import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.events.handler.{LinkEnterEventHandler, LinkLeaveEventHandler, PersonStuckEventHandler}
import org.matsim.api.core.v01.events.{LinkEnterEvent, LinkLeaveEvent, PersonStuckEvent}
import org.matsim.api.core.v01.network.Link

/**
  * keeps track of changes in the road network, based on events where vehicles enter and leave the network
  */
class RoadNetworkDeltaHandler(minNetworkUpdateThreshold: SimTime)
    extends LinkEnterEventHandler
    with LinkLeaveEventHandler
    with PersonStuckEventHandler {

  private var previousTimeUpdated: SimTime                      = SimTime.Zero
  private val linkDeltas: collection.mutable.Map[Id[Link], Int] = collection.mutable.Map.empty

  def clear(): Unit = this.linkDeltas.clear()

  /**
    * return the deltas. guard against null link ids (which is somehow possible). manages persistent data.
    *
    * @param currentTime the current time
    * @return deltas if we have reached a time to report them
    */
  def getDeltas(currentTime: SimTime): List[(EdgeId, Flow)] = {
    val timeToUpdateNetworkFlows: Boolean = currentTime - previousTimeUpdated >= minNetworkUpdateThreshold
    if (timeToUpdateNetworkFlows) {
      val deltas: List[(EdgeId, Flow)] =
        this.linkDeltas.toList
          .foldLeft(List.empty[(EdgeId, Flow)]) {
            case (acc, (linkId, marginalVehicleCounts)) =>
              if (linkId == null) {
                acc
              } else {
                (EdgeId(linkId.toString), Flow(marginalVehicleCounts)) +: acc
              }
          }
      this.clear()
      previousTimeUpdated = currentTime
      deltas
    } else List.empty
  }

  /**
    * increase count of a link due to a vehicle entering a link
    * @param event a matsim LinkEnterEvent
    */
  def handleEvent(event: LinkEnterEvent): Unit =
    this.linkDeltas.get(event.getLinkId) match {
      case None =>
        this.linkDeltas.update(event.getLinkId, 1)
      case Some(linkIdData) =>
        this.linkDeltas.update(event.getLinkId, linkIdData + 1)
    }

  /**
    * reduce count of a link due to a vehicle normally exiting a link
    * @param event
    */
  def handleEvent(event: LinkLeaveEvent): Unit =
    this.linkDeltas.get(event.getLinkId) match {
      case None =>
        this.linkDeltas.update(event.getLinkId, -1)
      case Some(linkIdData) =>
        this.linkDeltas.update(event.getLinkId, linkIdData - 1)
    }

  /**
    * remove a link flow count associated with a stuck person event
    * @param event
    */
  def handleEvent(event: PersonStuckEvent): Unit = {
    this.linkDeltas.get(event.getLinkId) match {
      case None =>
        this.linkDeltas.update(event.getLinkId, -1)
      case Some(linkIdData) =>
        this.linkDeltas.update(event.getLinkId, linkIdData - 1)
    }
  }
}

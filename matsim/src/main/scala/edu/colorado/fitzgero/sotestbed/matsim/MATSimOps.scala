package edu.colorado.fitzgero.sotestbed.matsim

import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.network.Link

object MATSimOps {
  /**
    * a MATSim path does not include start or end. this safely combines everything we have
    * related to this path assignment and turns it into a MATSim path
    *
    * @param start the agent's current location
    * @param end the destination of the agent's trip
    * @param pathPrefix a path from the start to a point that the routing algorithm treated as a start point
    * @param routingResult a new path solution to add to this agent, with consideration of the pathPrefix
    * @return all edges between but not including start and end on the new path
    */
  def coalescePath(start: Id[Link], end: Id[Link], pathPrefix: List[Id[Link]], routingResult: List[Id[Link]]): List[Id[Link]] = {
    // if there is a path prefix, it doesn't have the start edge
    val updatedPathPrefix: List[Id[Link]] =
      pathPrefix.headOption match {
        case None => List.empty
        case Some(head) =>
          if (start == head) pathPrefix.tail
          else pathPrefix
      }

    // if there is a routing result, it doesn't have the end edge
    val updatedRoutingResult: List[Id[Link]] =
      routingResult.lastOption match {
        case None => List.empty
        case Some(last) =>
          if (end == last) routingResult.init
          else routingResult
      }

    // combine the prefix and the routing result, making sure not to repeat the joined edge
    val combined: List[Id[Link]] =
      updatedPathPrefix.lastOption match {
        case None => updatedRoutingResult
        case Some(lastLinkInPathPrefix) =>
          updatedRoutingResult.headOption match {
            case None => updatedPathPrefix
            case Some(firstLinkInRoutingResult) =>
              if (lastLinkInPathPrefix == firstLinkInRoutingResult) updatedPathPrefix ++ updatedRoutingResult.tail
              else updatedPathPrefix ++ updatedRoutingResult
          }
      }

    combined
  }
}

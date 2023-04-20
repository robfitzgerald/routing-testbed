package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import cats.implicits._
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._

/**
  * associates a Batch with a group of edges in the road network.
  * these edges
  *   1. are observed for this Batch's congestion observation
  *   2. are used to group Agents
  *
  * @param batchId the unique identifier for this batch from the batching manager
  *                and batching function
  * @param edges
  */
final case class NetworkZone(batchId: String, edges: List[EdgeId])

object NetworkZone {

  implicit val enc: Decoder[List[NetworkZone]] =
    Decoder
      .decodeMap[String, List[String]]
      .emap { map =>
        map.toList.traverse {
          case (batchId, edges) =>
            Right(NetworkZone(batchId, edges.map(EdgeId.apply)))
        }
      }
}

package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

sealed trait CongestionObservation

object CongestionObservation {

  /**
    * observes only the travel time increase at the request link location
    * when calculating the observed congestion effect
    */
//  case object Disaggregate extends CongestionObservation

  /**
    * takes the mean congestion effect across a batch of requests
    */
  case object MeanFromBatch extends CongestionObservation

  /**
    * takes the max congestion effect across a batch of requests
    */
  case object MaxFromBatch extends CongestionObservation

  // ..others? weighted mean, by volume? or, expand the sample beyond the
  // request batch link locations to a broader neighborhood? or,
  // get the in-links/out-links for each?

  implicit class CongestionObservationExtensionMethods(c: CongestionObservation) {

    def observeCongestion(
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      marginalCostFunction: EdgeBPR => Flow => Cost,
      edges: List[EdgeId]
    ): IO[Double] = {

      val edgeDataF =
        edges.traverse { edgeId =>
          roadNetwork.edge(edgeId).flatMap {
            case None                => IO.raiseError(new Error(s"missing edge $edgeId"))
            case Some(edgeIdAndAttr) => IO.pure(edgeIdAndAttr)
          }
        }
      edgeDataF.map { edgeData =>
        val observations = for {
          edgeIdAndAttr <- edgeData
        } yield {
          val current  = marginalCostFunction(edgeIdAndAttr.attribute)(Flow.Zero)
          val ff       = edgeIdAndAttr.attribute.freeFlowCost
          val increase = (current - ff) / ff
          increase.value
        }

        c match {
          case MeanFromBatch =>
            if (observations.isEmpty) 0.0 else observations.sum / observations.length
          case MaxFromBatch =>
            if (observations.isEmpty) 0.0 else observations.max
        }
      }
    }
  }

}

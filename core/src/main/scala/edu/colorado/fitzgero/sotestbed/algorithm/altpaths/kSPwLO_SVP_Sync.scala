package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import scala.annotation.tailrec

import cats.Monad
import cats.data.OptionT
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.search.{DijkstraSearch, SpanningTree}
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, NaturalNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork._

/**
  *
  * @param theta percent that paths can be similar to each other, with default of 100% similarity
  * @tparam F
  * @tparam V
  * @tparam E
  */
class kSPwLO_SVP_Sync[F[_]: Monad, V, E](
  theta: Cost = Cost(1.0), // @TODO: percentage numeric type, or, numeric library brah
  retainSrcDstEdgesInPaths: Boolean = false
) extends AltPathsAlgorithm[F, V, E] {

  def generateAlts(
    requests: List[Request],
    roadNetwork: RoadNetwork[F, V, E],
    costFunction: E => Cost,
    terminationFunction: AltPathsAlgorithm.AltPathsState => Boolean
  ): F[AltPathsAlgorithm.AltPathsResult] = {
    if (requests.isEmpty) Monad[F].pure { AltPathsAlgorithm.AltPathsResult(Map.empty) }
    else {
      for {
        result <- requests.traverse { request =>
          kSPwLO_SVP_Algorithm.generateAltsForRequest(
            request,
            roadNetwork,
            costFunction,
            theta,
            terminationFunction
          )
        }
      } yield {
        AltPathsAlgorithm.AltPathsResult(
          result
            .flatten
            .map{ case kSPwLO_SVP_Algorithm.SingleSVPResult(req, alts, _) => req -> alts }
            .toMap
        )
      }
    }
  }
}

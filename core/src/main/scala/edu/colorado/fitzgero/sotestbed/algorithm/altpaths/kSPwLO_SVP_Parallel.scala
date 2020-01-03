package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import cats.implicits._
import cats.{Monad, Parallel}

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork._

/**
  *
  *
  * @param theta percent that paths can be similar to each other, with default of 100% similarity
  * @tparam F
  * @tparam V
  * @tparam E
  */
class kSPwLO_SVP_Parallel[F[_]: Monad: Parallel, V, E](
  k: Int,
  theta: Cost = Cost(1.0), // @TODO: percentage numeric type, or, numeric library brah
  val terminationFunction: KSPAlgorithm.AltPathsState => Boolean,
  retainSrcDstEdgesInPaths: Boolean = false
) extends KSPAlgorithm[F, V, E] {

  def generateAlts(
    requests: List[Request],
    roadNetwork: RoadNetwork[F, V, E],
    costFunction: E => Cost,
  ): F[KSPAlgorithm.AltPathsResult] = {
    if (requests.isEmpty) Monad[F].pure { KSPAlgorithm.AltPathsResult(Map.empty) } else {

      for {
        alts <- requests.parTraverse { request =>
          kSPwLO_SVP_Algorithm.generateAltsForRequest(
            request,
            roadNetwork,
            costFunction,
            theta,
            terminationFunction
          )
        }
      } yield {

        KSPAlgorithm.AltPathsResult(
          alts
            .flatten
            .map{ case kSPwLO_SVP_Algorithm.SingleSVPResult(req, alts, _) => req -> alts.take(k) }
            .toMap
        )
      }
    }
  }
}



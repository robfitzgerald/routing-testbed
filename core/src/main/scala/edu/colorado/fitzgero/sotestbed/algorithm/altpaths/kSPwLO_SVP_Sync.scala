package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import cats.Monad
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork._

/**
  *
  * @param theta percent that paths can be similar to each other, with default of 100% similarity
  * @tparam F
  * @tparam V
  * @tparam E
  */
class kSPwLO_SVP_Sync[F[_]: Monad, V, E](
  k: Int,
  theta: Cost = Cost(1.0),
  val terminationFunction: KSPAlgorithm.AltPathsState => Boolean,
  minBatchSize: Int = 2,
  retainSrcDstEdgesInPaths: Boolean = false
) extends KSPAlgorithm[F, V, E]
    with LazyLogging {

  def generateAlts(
    requests: List[Request],
    roadNetwork: RoadNetwork[F, V, E],
    costFunction: E => Cost
  ): F[KSPAlgorithm.AltPathsResult] = {
    if (requests.isEmpty) {
      logger.debug(s"kSPwLO no alts received")
      Monad[F].pure { KSPAlgorithm.AltPathsResult(Map.empty) }
    } else {
      // if we do not meet the user-specified minimum batch size, then override
      // with a simple true shortest path search
      val terminationFunctionForThisBatch: KSPAlgorithm.AltPathsState => Boolean =
        if (requests.length < minBatchSize) { state: KSPAlgorithm.AltPathsState =>
          state.pathsSeen == NonNegativeNumber.One
        } else {
          terminationFunction
        }
      for {
        result <- requests.traverse { request =>
          kSPwLO_SVP_Algorithm.generateAltsForRequest(
            request,
            roadNetwork,
            costFunction,
            theta,
            terminationFunctionForThisBatch
          )
        }
      } yield {
        val avgAlts: Double =
          if (result.isEmpty) 0d
          else result.flatMap { case Some(x) => Some(x.alts.size); case None => None }.sum.toDouble / result.size

        logger.debug(f"AVG $avgAlts%2f alts per agent")
        KSPAlgorithm.AltPathsResult(
          result.flatten.map { case kSPwLO_SVP_Algorithm.SingleSVPResult(req, alts, _) => req -> alts.take(k) }.toMap
        )
      }
    }
  }
}

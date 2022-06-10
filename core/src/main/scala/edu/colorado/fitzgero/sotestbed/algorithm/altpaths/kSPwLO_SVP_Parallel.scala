package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import cats.effect.IO
import cats.implicits._
import cats.{Monad, Parallel}

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, NonNegativeNumber, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

/**
  *
  *
  * @param theta percent that paths can be similar to each other, with default of 100% similarity
  */
class kSPwLO_SVP_Parallel(
  k: Int,
  theta: Cost = Cost(1.0), // @TODO: percentage numeric type, or, numeric library brah
  val terminationFunction: KSPAlgorithm.AltPathsState => Boolean,
  minBatchSize: Int = 2,
  retainSrcDstEdgesInPaths: Boolean = false
) extends KSPAlgorithm {

  def generateAlts(
    requests: List[Request],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    costFunction: EdgeBPR => Cost
  ): IO[KSPAlgorithm.AltPathsResult] = {
    if (requests.isEmpty) IO.pure { KSPAlgorithm.AltPathsResult(Map.empty) }
    else {
      // if we do not meet the user-specified minimum batch size, then override
      // with a simple true shortest path search
      val startTime: RunTime = RunTime.Now
      val terminationFunctionOverride: KSPAlgorithm.AltPathsState => Boolean =
        if (requests.length < minBatchSize) { state: KSPAlgorithm.AltPathsState =>
          state.pathsSeen == NonNegativeNumber.One
        } else {
          terminationFunction
        }
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
        val runTime: RunTime = RunTime.Now - startTime
        KSPAlgorithm.AltPathsResult(
          alts.flatten.map { case kSPwLO_SVP_Algorithm.SingleSVPResult(req, alts, _) => req -> alts.take(k) }.toMap,
          runtime = runTime
        )
      }
    }
  }
}

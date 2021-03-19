package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import scala.util.Random

import cats.Monad
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPFilter.KSPFilterFunction
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, PathSegment, RoadNetwork}

final case class AltPathsAlgorithmRunner[F[_]: Monad, V, E](
  altPathsAlgorithm: KSPAlgorithm[F, V, E],
  kspFilterFunction: KSPFilterFunction,
  costFunction: E => Cost,
  freeFlowCostFunction: E => Cost,
  useFreeFlowNetworkCostsInPathSearch: Boolean,
  seed: Long
) extends LazyLogging {

  def run(
    batchId: String,
    reqs: List[Request],
    activeAgentHistory: ActiveAgentHistory,
    roadNetwork: RoadNetwork[F, V, E]
  ): F[AltPathsAlgorithmResult] = {

    val rng: Random = new Random(seed)

    val overrideCostFunction: E => Cost =
      if (useFreeFlowNetworkCostsInPathSearch) freeFlowCostFunction else this.costFunction

    // run the KSP algorithm. if required, re-populate the KSP result's link costs.
    val result = for {
      altsResult <- altPathsAlgorithm.generateAlts(reqs, roadNetwork, overrideCostFunction)
      startTime  <- Monad[F].pure(RunTime(System.currentTimeMillis))
      altsWithCurrentCosts <- AltPathsAlgorithmRunner.handleAltsResultNetworkCosts(
        useFreeFlowNetworkCostsInPathSearch,
        altsResult,
        roadNetwork,
        costFunction
      )
    } yield {

      // apply the ksp filter function
      val altsWithFilterApplied: Map[Request, List[Path]] = altsWithCurrentCosts.alternatives.flatMap {
        case (req, alts) =>
          activeAgentHistory.observedRouteRequestData.get(req.agent) match {
            case None =>
              logger.warn(f"agent ${req.agent} with alts has no AgentHistory")
              Some(req -> alts)
            case Some(agentHistory) =>
              kspFilterFunction(agentHistory, req, alts, rng) match {
                case None =>
                  logger.debug(f"ksp filter fn removed agent ${req.agent}")
                  None
                case Some(filtered) =>
                  logger.debug(f"ksp filter processed agent ${req.agent}")
                  Some(filtered)
              }
          }
      }

      val filteredAlts: Option[Map[Request, List[Path]]] =
        if (altsWithFilterApplied.isEmpty) None else Some(altsWithFilterApplied)

      val endOfKspTime = RunTime(System.currentTimeMillis) - startTime + altsResult.runtime
      AltPathsAlgorithmResult(batchId, altsResult.alternatives, filteredAlts, endOfKspTime)
    }

    result
  }
}

object AltPathsAlgorithmRunner {

  final case class AltPathsAlgorithmResult(
    batchId: String,
    alts: Map[Request, List[Path]],
    filteredAlts: Option[Map[Request, List[Path]]],
    runtimeMilliseconds: RunTime
  )

  /**
    * if the user has specified to use free flow network costs in the path search,
    * then the result from the KSP algorithm will not have the current network costs.
    * in that case, this function will replace all link costs with current network costs.
    *
    * @param useFreeFlowNetworkCostsInPathSearch whether or not the user has specified to use free flow costs
    * @param altPathsResult the result of the KSP algorithm
    * @param roadNetwork the current road network costs
    * @param costFunction the cost function
    * @tparam F effect type, likely cats.effect.IO
    * @tparam V vertex attribute type
    * @tparam E edge attribute type
    * @return the alts with costs from the current network conditions
    */
  def handleAltsResultNetworkCosts[F[_]: Monad, V, E](
    useFreeFlowNetworkCostsInPathSearch: Boolean,
    altPathsResult: KSPAlgorithm.AltPathsResult,
    roadNetwork: RoadNetwork[F, V, E],
    costFunction: E => Cost
  ): F[KSPAlgorithm.AltPathsResult] = {
    val altsWithCurrentSpeeds: F[KSPAlgorithm.AltPathsResult] = if (useFreeFlowNetworkCostsInPathSearch) {
      val updatedAlts = for {
        (req, reqAlts) <- altPathsResult.alternatives
      } yield {
        val updatedReqAlts: List[F[List[PathSegment]]] = for {
          alt <- reqAlts
        } yield {
          alt.traverse { seg =>
            for {
              edgeOption <- roadNetwork.edge(seg.edgeId)
            } yield {
              edgeOption match {
                case None =>
                  seg
                case Some(edge) =>
                  seg.copy(cost = costFunction(edge.attribute))
              }
            }
          }
        }
        updatedReqAlts.sequence.map { alts => (req, alts) }
      }
      val result: F[KSPAlgorithm.AltPathsResult] =
        updatedAlts.toList.sequence.map { updatedAlts => altPathsResult.copy(alternatives = updatedAlts.toMap) }
      result
    } else {
      Monad[F].pure(altPathsResult)
    }
    altsWithCurrentSpeeds
  }

  case class AltsResultData(
    numBatches: Int = 0,
    avgBatchSize: Double = 0.0,
    avgPathTravelTimeSec: Double = 0.0,
    avgPathLinkCount: Double = 0.0
  )

  /**
    * logs reporting data on this batch's set of alternatives
    *
    * @param alts the set of batch alternatives (before filtering)
    * @return data on the batches
    */
  def logAltsResultData(alts: List[AltPathsAlgorithmRunner.AltPathsAlgorithmResult]): AltsResultData = {
    if (alts.isEmpty) AltsResultData()
    else {
      val numBatches   = alts.size
      val avgBatchSize = alts.map { _.alts.size.toDouble }.sum / numBatches
      val allPaths     = alts.flatMap { _.alts.values.flatten }
      val numPaths     = allPaths.length
      val (sumPathTravelTime, sumPathLinkCount) =
        allPaths.foldLeft((0.0, 0.0)) { (acc, path) =>
          val (accPathTravelTime, accPathLinkCount) = acc
          val thisPathTravelTime                    = if (path.isEmpty) 0.0 else path.map { _.cost.value }.sum
          val thisPathLinkCount                     = path.length
          (accPathTravelTime + thisPathTravelTime, accPathLinkCount + thisPathLinkCount)
        }
      val avgPathTravelTime = sumPathTravelTime / numPaths
      val avgPathLinkCount  = sumPathLinkCount / numPaths
      val result = AltsResultData(
        numBatches = numBatches,
        avgBatchSize = avgBatchSize,
        avgPathTravelTimeSec = avgPathTravelTime,
        avgPathLinkCount = avgPathLinkCount
      )
      result
    }
  }
}

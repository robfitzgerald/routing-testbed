package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import scala.util.Random

import cats.Monad
import cats.effect.IO
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPFilter.KSPFilterFunction
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, PathSegment, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.DriverPolicySpaceV2Ops
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork

final case class AltPathsAlgorithmRunner(
  altPathsAlgorithm: KSPAlgorithm,
  kspFilterFunction: KSPFilterFunction,
  costFunction: EdgeBPR => Cost,
  freeFlowCostFunction: EdgeBPR => Cost,
  useFreeFlowNetworkCostsInPathSearch: Boolean,
  seed: Long
) extends LazyLogging {

  def run(
    batchId: String,
    reqs: List[Request],
    activeAgentHistory: ActiveAgentHistory,
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR]
  ): IO[AltPathsAlgorithmResult] = {

    val rng: Random = new Random(seed)

    val overrideCostFunction: EdgeBPR => Cost =
      if (useFreeFlowNetworkCostsInPathSearch) freeFlowCostFunction else this.costFunction

    // run the KSP algorithm. if required, re-populate the KSP result's link costs.
    val result = for {
      altsResult <- altPathsAlgorithm.generateAlts(reqs, roadNetwork, overrideCostFunction)
      startTime  <- IO { RunTime(System.currentTimeMillis) }
      altsWithCurrentCosts <- AltPathsAlgorithmRunner.handleAltsResultNetworkCosts(
        useFreeFlowNetworkCostsInPathSearch,
        altsResult,
        roadNetwork,
        costFunction
      )
      altsNoLoop <- AltPathsAlgorithmRunner.removePathsWithLoops(altsWithCurrentCosts, activeAgentHistory, roadNetwork)
      altsSorted <- AltPathsAlgorithmRunner.reSortPathsForBatch(altsNoLoop, activeAgentHistory, roadNetwork)
    } yield {

      // apply the ksp filter function
      val altsWithFilterApplied: Map[Request, List[Path]] = altsSorted.alternatives.flatMap {
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
                case Some((req, Nil)) =>
                  logger.debug(f"ksp filter returned request with no paths for agent ${req.agent}")
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
      AltPathsAlgorithmResult(batchId, altsSorted.alternatives, filteredAlts, endOfKspTime)
    }

    result
  }
}

object AltPathsAlgorithmRunner extends LazyLogging {

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

  def removePathsWithLoops(
    altsResult: KSPAlgorithm.AltPathsResult,
    activeAgentHistory: ActiveAgentHistory,
    rn: RoadNetwork[IO, Coordinate, EdgeBPR]
  ): IO[KSPAlgorithm.AltPathsResult] = {
    val removeLoopsResult = altsResult.alternatives.toList
      .traverse {
        case (req, alts) =>
          // extract our agent's most recent trip from the agent history and use it
          // to reconstruct what we expect the underlying simulation will assign as the
          // modified route plan.
          // transform our Path into a List[EdgeData] to leverage the coalesceFuturePath method
          for {
            hist     <- IO.fromEither(activeAgentHistory.getAgentHistoryOrError(req.agent))
            current  <- IO.fromEither(hist.currentRequest)
            altEdges <- alts.traverse { alt => alt.traverse(_.toEdgeData(rn)).map { edges => (alt, edges) } }
          } yield {
            val (loopsRemoved, _) = altEdges.filter {
              case (alt, edges) =>
                val rem     = DriverPolicySpaceV2Ops.coalesceFuturePath(current.remainingRoute, edges)
                val full    = DriverPolicySpaceV2Ops.coalesceFuturePath(current.experiencedRoute, rem)
                val fullIds = full.map { _.edgeId }
                val noLoop  = fullIds.toSet.size == fullIds.length
                noLoop
            }.unzip

            logger.whenInfoEnabled {
              val nLoopy = altEdges.length - loopsRemoved.length
              if (nLoopy > 0) {
                logger.info(f"removed $nLoopy paths for agent ${req.agent} that had loops when coalesced")
              }
            }

            (req, loopsRemoved)
          }
      }
      .map { _.toMap }

    removeLoopsResult.map { noLoops => altsResult.copy(alternatives = noLoops) }
  }

  /**
    * after computing a batch of alternatives, we attach each to the current trip plan
    * and find out the sort ordering. each cost should estimate the full path plus
    * the path spur. the resulting sorted path lists make it so that the 0th path
    * is the most user-optimal, and the remaining paths appear in increasing system-
    * optimizing order, so that the last path is most-system-optimizing.
    *
    * @param altsResult the path alternatives to sort
    * @param activeAgentHistory dataset providing the current trip estimate for each agent
    * @return the path alternatives re-sorted
    */
  def reSortPathsForBatch(
    altsResult: KSPAlgorithm.AltPathsResult,
    activeAgentHistory: ActiveAgentHistory,
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]
  ): IO[KSPAlgorithm.AltPathsResult] = {
    val sortResult = altsResult.alternatives.toList
      .traverse {
        case (req, alts) =>
          IO.fromEither(activeAgentHistory.getAgentHistoryOrError(req.agent))
            .flatMap { hist =>
              alts
                .traverse { path =>
                  DriverPolicySpaceV2Ops
                    .pathAlternativeTravelTimeEstimate(rn, hist, path)
                    .map { cost => (path, cost) }
                }
                .map { pathsWithCosts =>
                  val sorted = pathsWithCosts.sortBy { case (_, cost) => cost }.map { case (path, _) => path }
                  (req, sorted)
                }
            }
      }
      .map { _.toMap }

    sortResult.map { sortedAlts => altsResult.copy(alternatives = sortedAlts) }
  }

  case class AltsResultData(
    numBatches: Int = 0,
    avgBatchSize: Double = 0.0,
    avgPathTravelTimeSec: Double = 0.0,
    avgPathLinkCount: Double = 0.0,
    totalRuntimeMs: RunTime = RunTime.Zero
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
      val overallRuntimeMs = RunTime(alts.map { _.runtimeMilliseconds.value }.sum)
      val numBatches       = alts.size
      val avgBatchSize     = alts.map { _.alts.size.toDouble }.sum / numBatches
      val allPaths         = alts.flatMap { _.alts.values.flatten }
      val numPaths         = allPaths.length
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
        avgPathLinkCount = avgPathLinkCount,
        totalRuntimeMs = overallRuntimeMs
      )
      result
    }
  }
}

package edu.colorado.fitzgero.sotestbed.algorithm.routing

import scala.util.Random

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPFilter.KSPFilterFunction
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner

/**
  * as the underlying MCTS library enforces/misuses IO, this class accomodates for that
  * by enforcing the IO dependency and being a proxy for the unsafe run context for
  * the MCTS library.
  *
  * @param altPathsAlgorithm
  * @param selectionAlgorithm
  * @param pathToMarginalFlowsFunction
  * @param combineFlowsFunction
  * @param marginalCostFunction
  * @param kspFilterFunction
  * @param useFreeFlowNetworkCostsInPathSearch
  * @param timeLimit
  * @param limitAltsRuntime
  * @param limitSelectionRuntime
  */
class TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm(
  altPathsAlgorithm: KSPAlgorithm,
  selectionAlgorithm: SelectionAlgorithm,
  pathToMarginalFlowsFunction: RoutingOps.PathToMarginalFlows[IO, Coordinate, EdgeBPR],
  combineFlowsFunction: Iterable[Flow] => Flow,
  marginalCostFunction: EdgeBPR => Flow => Cost,
  kspFilterFunction: KSPFilterFunction,
  useFreeFlowNetworkCostsInPathSearch: Boolean,
  minimumAverageImprovement: Cost,
  minBatchSize: Int,
  seed: Long,
  timeLimit: RunTime = RunTime(31536000), // one year.
  limitAltsRuntime: Boolean = true,
  limitSelectionRuntime: Boolean = true
) extends RoutingAlgorithm[IO, Coordinate, EdgeBPR]
    with LazyLogging {

  val rng: Random = new Random(seed)

  final override def route(
    reqs: List[Request],
    activeAgentHistory: ActiveAgentHistory,
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR]
  ): IO[RoutingAlgorithm.Result] = {

    if (reqs.size < minBatchSize) {
      val ignoredAgents: String = reqs.map { _.agent }.mkString(",")
      logger.info(s"ignoring batch with less than the minimum size of $minBatchSize for agents: $ignoredAgents")
      IO { RoutingAlgorithm.Result() }
    } else {

      val startTime: RunTime = RunTime(System.currentTimeMillis)
      val costFunction: EdgeBPR => Cost =
        if (useFreeFlowNetworkCostsInPathSearch) e => marginalCostFunction(e)(Flow.Zero)
        else e => marginalCostFunction(e)(e.flow)

      for {
        altsResult <- altPathsAlgorithm.generateAlts(reqs, roadNetwork, costFunction)
        altsFiltered <- AltPathsAlgorithmRunner.applyKspFilter(
          altsResult.alternatives,
          kspFilterFunction,
          activeAgentHistory,
          roadNetwork,
          rng
        )
        endOfKspTime = RunTime(System.currentTimeMillis)
        kspRuntime   = endOfKspTime - startTime
      } yield {
        // // first, apply the ksp filter function
        // val filteredAlts: Map[Request, List[Path]] = altsResult.alternatives.flatMap {
        //   case (req, alts) =>
        //     activeAgentHistory.observedRouteRequestData.get(req.agent) match {
        //       case None =>
        //         logger.warn(f"agent ${req.agent} with alts has no AgentHistory")
        //         Some { req -> alts }
        //       case Some(agentHistory) =>
        //         kspFilterFunction(roadNetwork, agentHistory, req, alts, rng) match {
        //           case None =>
        //             logger.debug(f"ksp filter fn removed agent ${req.agent}")
        //             None
        //           case Some(filtered) =>
        //             logger.debug(f"ksp filter processed agent ${req.agent}")
        //             Some { filtered }
        //         }
        //     }
        // }

        val selectionResult: SelectionAlgorithm.SelectionAlgorithmResult =
          selectionAlgorithm
            .selectRoutes(
              "unbatched",
              altsFiltered.getOrElse(Map.empty),
              roadNetwork,
              Map.empty,
              pathToMarginalFlowsFunction,
              combineFlowsFunction,
              marginalCostFunction
            )
            .unsafeRunSync()

        // minimumAverageImprovement
        if (math.abs(selectionResult.averageTravelTimeDiff.value) < minimumAverageImprovement.value) {
          val ignoredAgents: String = reqs.map { _.agent }.mkString(",")
          logger.info(
            s"ignoring batch with avg improvement ${selectionResult.averageTravelTimeDiff}s less than min required ${minimumAverageImprovement}s for agents: $ignoredAgents"
          )
          RoutingAlgorithm.Result()
        } else {
          val selectionResultWithKSPPaths: List[Response] =
            TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm.useKSPResultPaths(
              selectionResult.selectedRoutes,
              altsResult.alternatives
            )

          val selectionRuntime = RunTime(System.currentTimeMillis) - endOfKspTime

          val result = RoutingAlgorithm.Result(
            altsResult.alternatives,
            altsFiltered.getOrElse(Map.empty),
            selectionResultWithKSPPaths,
            activeAgentHistory,
            kspRuntime,
            selectionRuntime,
            travelTimeDiff = selectionResult.travelTimeDiff,
            meanTravelTimeDiff = selectionResult.averageTravelTimeDiff,
            samples = selectionResult.samples.value
          )

          result
        }
      }
    }
  }
}

object TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm {

  /**
    * the ksp filter may have modified the path that we will assign; this lets us override the
    * selection result (due to the ksp filter) with the (complete) path that the ksp algorithm found
    * @param selectionResponses the responses for this routing
    * @param altsResults the ksp algorithm response
    * @return Responses with the complete ksp route due to selection
    */
  def useKSPResultPaths(selectionResponses: List[Response], altsResults: Map[Request, List[Path]]): List[Response] = {
    for {
      selectionResponse <- selectionResponses
      alts              <- altsResults.get(selectionResponse.request)
    } yield {
      selectionResponse.copy(
        path = alts(selectionResponse.pathIndex).map { _.edgeId }
      )
    }
  }
}

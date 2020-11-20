package edu.colorado.fitzgero.sotestbed.algorithm.selection

import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{RoutingOps, TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionAlgorithmResult
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionRunner.SelectionRunnerResult
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}

final case class SelectionRunner[V](
  selectionAlgorithm: SelectionAlgorithm[IO, V, EdgeBPR],
  pathToMarginalFlowsFunction: RoutingOps.PathToMarginalFlows[IO, V, EdgeBPR],
  combineFlowsFunction: Iterable[Flow] => Flow,
  marginalCostFunction: EdgeBPR => Flow => Cost,
  minimumAverageImprovement: Cost
) extends LazyLogging {

  /**
    * select the best combination of paths from a set of alternative paths given the
    * provided road network conditions
    *
    * @param filteredAlts the alternative paths for a set of requests
    * @param roadNetwork the road network state
    * @return the best combination of paths as [[Response]] objects for each [[Request]]
    */
  def run(
    batchId: String,
    filteredAlts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[IO, V, EdgeBPR]
  ): IO[Option[SelectionRunnerResult]] = {

    val startTime = RunTime(System.currentTimeMillis)

    val selectionResultIO: IO[SelectionAlgorithm.SelectionAlgorithmResult] =
      selectionAlgorithm
        .selectRoutes(
          filteredAlts,
          roadNetwork,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction
        )

    val result = for {
      selectionResult <- selectionResultIO
    } yield {

      // test for minimumAverageImprovement threshold
      if (math.abs(selectionResult.averageTravelTimeDiff.value) < minimumAverageImprovement.value) {
        val ignoredAgents: String = filteredAlts.keys
          .map {
            _.agent
          }
          .mkString(",")
        logger.info(
          s"ignoring batch with avg improvement ${selectionResult.averageTravelTimeDiff}s less than min required ${minimumAverageImprovement}s for agents: $ignoredAgents"
        )
        None
      } else {
        val selectionResultWithKSPPaths: List[Response] =
          TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm.useKSPResultPaths(
            selectionResult.selectedRoutes,
            filteredAlts
          )
        val selectionAlgorithmResult = selectionResult.copy(selectedRoutes = selectionResultWithKSPPaths)

        val selectionRuntime = startTime - RunTime(System.currentTimeMillis)

        Some(SelectionRunnerResult(batchId, selectionAlgorithmResult, selectionRuntime))
      }
    }

    result
  }
}

object SelectionRunner {

  final case class SelectionRunnerResult(
    batchId: String,
    selection: SelectionAlgorithmResult,
    runtime: RunTime
  )

  /**
    * the ksp filter may have modified the path that we will assign; this lets us override the
    * selection result (due to the ksp filter) with the (complete) path that the ksp algorithm found
    * @param selectionResponses the responses for this routing
    * @param altsResults the ksp algorithm response, from before the ksp filter step
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

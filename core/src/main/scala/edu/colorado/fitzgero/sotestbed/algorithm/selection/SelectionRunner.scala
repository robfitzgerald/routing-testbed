package edu.colorado.fitzgero.sotestbed.algorithm.selection

import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{
  FlowObservationOps,
  TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm
}
import edu.colorado.fitzgero.sotestbed.algorithm.selection._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

final case class SelectionRunner(
  selectionAlgorithm: SelectionAlgorithm,
  pathToMarginalFlowsFunction: FlowObservationOps.PathToMarginalFlows[IO, Coordinate, EdgeBPR],
  combineFlowsFunction: Iterable[Flow] => Flow,
  marginalCostFunction: EdgeBPR => Flow => Cost,
  minimumAverageImprovement: Cost
) extends LazyLogging {

  /**
    * select the best combination of paths from a set of alternative paths given the
    * provided road network conditions
    *
    * @param req the alternative paths for a set of requests
    * @param roadNetwork the road network state
    * @return the best combination of paths as [[Response]] objects for each [[Request]]
    */
  def run(
    req: SelectionRunnerRequest,
    currentSimTime: SimTime,
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    bank: Map[String, Karma]
  ): IO[Option[(SelectionRunnerResult, Map[String, Karma])]] = {

    val startTime = RunTime(System.currentTimeMillis)

    val selectionResultIO: IO[SelectionAlgorithm.SelectionAlgorithmResult] =
      selectionAlgorithm
        .selectRoutes(
          req.batchId,
          req.finalAlternatePaths,
          currentSimTime,
          roadNetwork,
          bank,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction
        )

    val result = for {
      selectionResult <- selectionResultIO
    } yield {

      // test for minimumAverageImprovement threshold
      if (math.abs(selectionResult.averageTravelTimeDiff.value) < minimumAverageImprovement.value) {
        val ignoredAgents: String = req.finalAlternatePaths.keys
          .map { _.agent }
          .mkString(",")
        logger.info(
          s"ignoring batch with avg improvement ${selectionResult.averageTravelTimeDiff}s less than min required ${minimumAverageImprovement}s for agents: $ignoredAgents"
        )
        None
      } else {
        val selectionResultWithKSPPaths: List[Response] =
          TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm.useKSPResultPaths(
            selectionResult.selectedRoutes,
            req.finalAlternatePaths
          )
        val selectionAlgorithmResult = selectionResult.copy(selectedRoutes = selectionResultWithKSPPaths)

        val selectionRuntime = RunTime(System.currentTimeMillis) - startTime

        Some(
          (
            SelectionRunnerResult(req.batchId, selectionAlgorithmResult, selectionRuntime),
            selectionResult.updatedBank
          )
        )
      }
    }

    result
  }
}

object SelectionRunner {

  /**
    * the ksp filter may have modified the path that we will assign; this lets us override the
    * selection result (due to the ksp filter) with the (complete) path that the ksp algorithm found
    * @param selectionResponses the responses for this routing
    * @param altsResults the ksp algorithm response, from before the ksp filter step
    * @return Responses with the complete ksp route due to selection
    */
  def handlePathsForResponses(
    selectionResponses: List[Response],
    altsResults: Map[Request, List[Path]]
  ): List[Response] = {
    for {
      selectionResponse <- selectionResponses
      alts              <- altsResults.get(selectionResponse.request)
      if selectionResponse.pathIndex != 0 // remove current path assignments, the agents are already on that path!
    } yield {
      selectionResponse.copy(
        path = alts(selectionResponse.pathIndex).map { _.edgeId }
      )
    }
  }
}

package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats.Monad
import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

/**
  *
  * @param altPathsAlgorithm
  * @param selectionAlgorithm
  * @param pathToMarginalFlowsFunction
  * @param combineFlowsFunction
  * @param marginalCostFunction
  * @param timeLimit
  * @param limitAltsRuntime
  * @param limitSelectionRuntime
  * @deprecated replaced with ksp filter version
  */
class TwoPhaseRoutingAlgorithm(
  altPathsAlgorithm: KSPAlgorithm,
  selectionAlgorithm: SelectionAlgorithm,
  pathToMarginalFlowsFunction: FlowObservationOps.PathToMarginalFlows[IO, Coordinate, EdgeBPR],
  combineFlowsFunction: Iterable[Flow] => Flow,
  marginalCostFunction: EdgeBPR => Flow => Cost,
  timeLimit: RunTime = RunTime(31536000), // one year.
  limitAltsRuntime: Boolean = true,
  limitSelectionRuntime: Boolean = true
) extends RoutingAlgorithm[IO, Coordinate, EdgeBPR] {

  final override def route(
    reqs: List[Request],
    activeAgentHistory: ActiveAgentHistory,
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR]
  ): IO[RoutingAlgorithm.Result] = {

    val startTime: RunTime            = RunTime(System.currentTimeMillis)
    val costFunction: EdgeBPR => Cost = e => marginalCostFunction(e)(Flow.Zero)

    if (reqs.isEmpty) {
      IO.pure(RoutingAlgorithm.Result())
    } else {
      for {
        altsResult <- altPathsAlgorithm.generateAlts(reqs, roadNetwork, costFunction)
        kspRuntime = RunTime(System.currentTimeMillis) - startTime
        selectionResult <- selectionAlgorithm.selectRoutes(
          "unbatched",
          altsResult.alternatives,
          roadNetwork,
          Map.empty,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction
        )
        selectionRuntime = RunTime(System.currentTimeMillis) - kspRuntime
      } yield {

        RoutingAlgorithm.Result(
          altsResult.alternatives,
          Map.empty,                      // update w/ ksp filter
          selectionResult.selectedRoutes, // when ksp filter, be sure to add TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm.useKSPResultPaths
          activeAgentHistory,
          kspRuntime,
          selectionRuntime,
          travelTimeDiff = selectionResult.travelTimeDiff,
          meanTravelTimeDiff = selectionResult.averageTravelTimeDiff,
          samples = selectionResult.samples.value
        )
      }
    }
  }
}

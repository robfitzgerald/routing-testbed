package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats.effect.SyncIO

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR

/**
  * as the underlying MCTS library enforces/misuses IO, this class accomodates for that
  * by enforcing the SyncIO dependency and being a proxy for the unsafe run context for
  * the MCTS library.
  *
  * @param altPathsAlgorithm
  * @param selectionAlgorithm
  * @param pathToMarginalFlowsFunction
  * @param combineFlowsFunction
  * @param marginalCostFunction
  * @param useFreeFlowNetworkCostsInPathSearch
  * @param timeLimit
  * @param limitAltsRuntime
  * @param limitSelectionRuntime
  * @tparam V vertex type
  */
class TwoPhaseLocalMCTSEdgeBPRRoutingAlgorithm[V](
  altPathsAlgorithm: KSPAlgorithm[SyncIO, V, EdgeBPR],
  selectionAlgorithm: SelectionAlgorithm[SyncIO, V, EdgeBPR],
  pathToMarginalFlowsFunction: RoutingOps.PathToMarginalFlows[SyncIO, V, EdgeBPR],
  combineFlowsFunction: Iterable[Flow] => Flow,
  marginalCostFunction: EdgeBPR => Flow => Cost,
  useFreeFlowNetworkCostsInPathSearch: Boolean,
  timeLimit: RunTime = RunTime(31536000), // one year.
  limitAltsRuntime: Boolean = true,
  limitSelectionRuntime: Boolean = true,
) extends RoutingAlgorithm[SyncIO, V, EdgeBPR] {

  final override def route(reqs: List[Request],
                           activeAgentHistory: ActiveAgentHistory,
                           roadNetwork: RoadNetwork[SyncIO, V, EdgeBPR]): SyncIO[RoutingAlgorithm.Result] = {

    val startTime: RunTime = RunTime(System.currentTimeMillis)
    val costFunction: EdgeBPR => Cost =
      if (useFreeFlowNetworkCostsInPathSearch) e => marginalCostFunction(e)(Flow.Zero)
      else e => marginalCostFunction(e)(e.flow)

    if (reqs.isEmpty) {
      SyncIO { RoutingAlgorithm.Result() }
    } else {
      for {
        altsResult <- altPathsAlgorithm.generateAlts(reqs, roadNetwork, costFunction)
        endOfKspTime = RunTime(System.currentTimeMillis)
        kspRuntime   = endOfKspTime - startTime
      } yield {
        val selectionResult: SelectionAlgorithm.Result = selectionAlgorithm
          .selectRoutes(altsResult.alternatives, roadNetwork, pathToMarginalFlowsFunction, combineFlowsFunction, marginalCostFunction)
          .unsafeRunSync()
        val selectionRuntime = RunTime(System.currentTimeMillis) - endOfKspTime
        RoutingAlgorithm.Result(
          altsResult.alternatives,
          Map.empty, // update with ksp filter
          selectionResult.selectedRoutes,
          activeAgentHistory,
          kspRuntime,
          selectionRuntime
        )
      }
    }
  }
}

package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats.effect.SyncIO

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

/**
  * as the underlying MCTS library enforces/misuses IO, this class accomodates for that
  * by enforcing the SyncIO dependency and being a proxy for the unsafe run context for
  * the MCTS library.
  * @param altPathsAlgorithm
  * @param selectionAlgorithm
  * @param pathToMarginalFlowsFunction
  * @param combineFlowsFunction
  * @param marginalCostFunction
  * @param timeLimit
  * @param limitAltsRuntime
  * @param limitSelectionRuntime
  * @tparam V vertex type
  * @tparam E edge type
  */
class TwoPhaseLocalMCTSRoutingAlgorithm[V, E](
  altPathsAlgorithm: KSPAlgorithm[SyncIO, V, E],
  selectionAlgorithm: SelectionAlgorithm[SyncIO, V, E],
  pathToMarginalFlowsFunction: RoutingOps.PathToMarginalFlows[SyncIO, V, E],
  combineFlowsFunction: Iterable[Flow] => Flow,
  marginalCostFunction: E => Flow => Cost,
  timeLimit: RunTime = RunTime(31536000), // one year.
  limitAltsRuntime: Boolean = true,
  limitSelectionRuntime: Boolean = true
) extends RoutingAlgorithm[SyncIO, V, E] {

  final override def route(reqs: List[Request],
                           activeAgentHistory: ActiveAgentHistory,
                           roadNetwork: RoadNetwork[SyncIO, V, E]): SyncIO[RoutingAlgorithm.Result] = {

    val startTime: RunTime      = RunTime(System.currentTimeMillis)
    val costFunction: E => Cost = e => marginalCostFunction(e)(Flow.Zero)

    if (reqs.isEmpty) {
      SyncIO { RoutingAlgorithm.Result() }
    } else {
      for {
        altsResult <- altPathsAlgorithm.generateAlts(reqs, roadNetwork, costFunction)
        kspRuntime = RunTime(System.currentTimeMillis) - startTime
      } yield {
        val selectionResult: SelectionAlgorithm.Result = selectionAlgorithm
          .selectRoutes(altsResult.alternatives, roadNetwork, pathToMarginalFlowsFunction, combineFlowsFunction, marginalCostFunction)
          .unsafeRunSync()
        val selectionRuntime = RunTime(System.currentTimeMillis) - kspRuntime
        RoutingAlgorithm.Result(
          altsResult.alternatives,
          Map.empty, // update w/ ksp filter
          selectionResult.selectedRoutes,
          kspRuntime,
          selectionRuntime
        )
      }
    }
  }
}

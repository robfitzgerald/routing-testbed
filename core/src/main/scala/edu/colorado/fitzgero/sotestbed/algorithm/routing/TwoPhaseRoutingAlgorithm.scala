package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats.Monad
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

class TwoPhaseRoutingAlgorithm[F[_]: Monad, V, E](
  altPathsAlgorithm: KSPAlgorithm[F, V, E],
  selectionAlgorithm: SelectionAlgorithm[F, V, E],
  pathToMarginalFlowsFunction: RoutingOps.PathToMarginalFlows[F, V, E],
  combineFlowsFunction: Iterable[Flow] => Flow,
  marginalCostFunction: E => Flow => Cost,
  timeLimit: RunTime = RunTime(31536000), // one year.
  limitAltsRuntime: Boolean = true,
  limitSelectionRuntime: Boolean = true
) extends RoutingAlgorithm[F, V, E] {

  final override def route(reqs: List[Request],
                           roadNetwork: RoadNetwork[F, V, E]): F[RoutingAlgorithm.Result] = {

    val startTime: RunTime      = RunTime(System.currentTimeMillis)
    val costFunction: E => Cost = e => marginalCostFunction(e)(Flow.Zero)

    if (reqs.isEmpty) {
      Monad[F].pure(RoutingAlgorithm.Result())
    } else {
      for {
        altsResult <- altPathsAlgorithm.generateAlts(reqs, roadNetwork, costFunction)
        kspRuntime = RunTime(System.currentTimeMillis) - startTime
        selectionResult <- selectionAlgorithm.selectRoutes(altsResult.alternatives,
          roadNetwork,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction)
        selectionRuntime = RunTime(System.currentTimeMillis) - kspRuntime
      } yield {
        RoutingAlgorithm.Result(
          altsResult.alternatives,
          selectionResult.selectedRoutes,
          kspRuntime,
          selectionRuntime
        )
      }
    }
  }
}

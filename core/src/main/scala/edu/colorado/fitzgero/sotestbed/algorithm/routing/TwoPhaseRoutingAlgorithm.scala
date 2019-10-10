package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats.Monad
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

class TwoPhaseRoutingAlgorithm[F[_]: Monad, V, E](
    altPathsAlgorithm: AltPathsAlgorithm[F, V, E],
    selectionAlgorithm: SelectionAlgorithm[F, V, E],
    pathToMarginalFlowsFunction: (RoadNetwork[F, V, E], Path) => F[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost,
    kspTerminationFunction: AltPathsAlgorithm.AltPathsState => Boolean,
    selectionTerminationFunction: SelectionAlgorithm.SelectionState => Boolean,
    timeLimit: RunTime = RunTime(31536000), // one year.
    limitAltsRuntime: Boolean = true,
    limitSelectionRuntime: Boolean = true
) extends RoutingAlgorithm[F, V, E] {

  final override def route(reqs: List[Request],
                           roadNetwork: RoadNetwork[F, V, E]): F[RoutingAlgorithm.Result] = {

    val startTime: RunTime      = RunTime(System.currentTimeMillis)
    val costFunction: E => Cost = e => marginalCostFunction(e)(Flow.Zero)

    for {
      altsResult <- altPathsAlgorithm.generateAlts(reqs,
                                                   roadNetwork,
                                                   costFunction,
                                                   kspTerminationFunction)
      kspRuntime = RunTime(System.currentTimeMillis) - startTime
      selectionResult <- selectionAlgorithm.selectRoutes(altsResult.alternatives,
                                                         roadNetwork,
                                                         pathToMarginalFlowsFunction,
                                                         combineFlowsFunction,
                                                         marginalCostFunction,
                                                         selectionTerminationFunction)
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

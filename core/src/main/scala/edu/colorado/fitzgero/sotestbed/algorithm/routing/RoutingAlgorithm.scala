package edu.colorado.fitzgero.sotestbed.algorithm.routing

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltsResultData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.model.agent._
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}

trait RoutingAlgorithm[F[_], V, E] {

  def route(
    requests: List[Request],
    activeAgentHistory: ActiveAgentHistory,
    roadNetwork: RoadNetwork[F, V, E]
  ): F[RoutingAlgorithm.Result]
}

object RoutingAlgorithm {

  /**
    * Wraps the response from any Routing Algorithm. Default case is reserved for
    * Empty route requests.
    *
    * @param kspResult the alternatives generated
    * @param filteredKspResult the alternatives after applying the [[edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPFilter.KSPFilterFunction]]
    * @param responses route solutions for each [[Request]]-ing agent
    * @param kspRuntime optional runtime for any kSP algorithm run
    * @param selectionRuntime optional runtime for any selection algorithm run
    */
  case class Result(
    kspResult: Map[Request, List[Path]] = Map.empty,
    filteredKspResult: Map[Request, List[Path]] = Map.empty,
    responses: List[Response] = List.empty,
    agentHistory: ActiveAgentHistory = ActiveAgentHistory(),
    kspRuntime: RunTime = RunTime.Zero,
    batchingRuntime: RunTime = RunTime.Zero,
    selectionRuntime: RunTime = RunTime.Zero,
    altsResultData: AltsResultData = AltsResultData(),
    selfishCost: Cost = Cost.Zero,
    optimalCost: Cost = Cost.Zero,
    travelTimeDiff: Cost = Cost.Zero,
    meanTravelTimeDiff: Cost = Cost.Zero,
    samples: Int = 0
  ) {

    def percentTTDiff: String = {
      if (selfishCost == Cost.Zero) "0%"
      else {
        val pct: Double = ((optimalCost.value - selfishCost.value) / selfishCost.value) * 100.0
        f"$pct%.2f%%"
      }
    }
  }
}

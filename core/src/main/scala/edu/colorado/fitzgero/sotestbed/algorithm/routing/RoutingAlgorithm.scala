package edu.colorado.fitzgero.sotestbed.algorithm.routing

import edu.colorado.fitzgero.sotestbed.model.agent._
import edu.colorado.fitzgero.sotestbed.model.numeric.RunTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}

trait RoutingAlgorithm[F[_], V, E] {
  def route(requests: List[Request], roadNetwork: RoadNetwork[F, V, E]): F[RoutingAlgorithm.Result]
}

object RoutingAlgorithm {

  /**
    * Wraps the response from any Routing Algorithm. Default case is reserved for
    * Empty route requests.
    *
    * @param alternatives the alternatives generated
    * @param responses route solutions for each [[Request]]-ing agent
    * @param kspRuntime optional runtime for any kSP algorithm run
    * @param selectionRuntime optional runtime for any selection algorithm run
    */
  case class Result(
    alternatives: Map[Request, List[Path]] = Map.empty,
    responses: List[Response] = List.empty,
    kspRuntime: RunTime = RunTime.Zero,
    selectionRuntime: RunTime = RunTime.Zero
  )

}
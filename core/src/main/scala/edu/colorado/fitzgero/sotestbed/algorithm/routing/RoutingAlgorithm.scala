package edu.colorado.fitzgero.sotestbed.algorithm.routing

import edu.colorado.fitzgero.sotestbed.model.agent._
import edu.colorado.fitzgero.sotestbed.model.numeric.RunTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetworkState

trait RoutingAlgorithm[V, E] {
  def route(requests: List[Request], roadNetworkModel: RoadNetworkState[V, E]): RoutingAlgorithm.Result
}

object RoutingAlgorithm {

  /**
    * Wraps the response from any Routing Algorithm
    *
    * @param responses route solutions for each [[Request]]-ing agent
    * @param kspRuntime optional runtime for any kSP algorithm run
    * @param selectionRuntime optional runtime for any selection algorithm run
    */
  case class Result(
    responses: List[Response],
    kspRuntime: Option[RunTime],
    selectionRuntime: Option[RunTime]
  )

}
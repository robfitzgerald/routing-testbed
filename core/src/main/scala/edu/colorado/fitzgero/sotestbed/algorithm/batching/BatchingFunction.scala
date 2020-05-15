package edu.colorado.fitzgero.sotestbed.algorithm.batching

import cats.Monad

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.Batching.{BatchingInstruction, BatchingStrategy}
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

trait BatchingFunction {

  /**
    * takes the current batching strategy and any updates about replan-able agents, and spits out an
    * update to that batching strategy
    *
    * @param roadNetwork the current road network state
    * @param activeRouteRequests agents which are available for SO routing requests
    * @param currentTime the current sim time
    * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
    */
  def updateBatchingStrategy[F[_]: Monad, V, E](roadNetwork: RoadNetwork[F, V, E],
                                                activeRouteRequests: List[RouteRequestData],
                                                currentTime: SimTime): F[Option[List[(String, List[Request])]]]

}

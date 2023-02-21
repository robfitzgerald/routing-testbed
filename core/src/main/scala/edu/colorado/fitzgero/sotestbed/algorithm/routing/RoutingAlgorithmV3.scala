package edu.colorado.fitzgero.sotestbed.algorithm.routing

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetworkIO
import edu.colorado.fitzgero.sotestbed.algorithm.batching.BatchingManager
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import cats.effect.IO

trait RoutingAlgorithmV3 {

  /**
    * route a set of requests using some underlying routing algorithm.
    *
    * @param roadNetwork current road network state
    * @param requests requests for routing from a batch or sub-batch
    * @param currentSimTime the current time in the simulation
    * @param batchingManager service tracking batching information
    * @param bank the balances of each agent in the system
    * @return routing results for each batch along with the updated bank
    */
  def route(
    roadNetwork: RoadNetworkIO,
    requests: List[RouteRequestData],
    currentSimTime: SimTime,
    batchingManager: BatchingManager,
    bank: Map[String, Karma]
  ): IO[(List[(String, RoutingAlgorithm.Result)], Map[String, Karma])]
}

package edu.colorado.fitzgero.sotestbed.algorithm.batching

import cats.Monad
import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.Batching.{BatchingInstruction, BatchingStrategy}
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

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
  def updateBatchingStrategy(
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    activeRouteRequests: List[RouteRequestData],
    currentTime: SimTime
  ): IO[Option[BatchingFunction.BatchingResult]]

}

object BatchingFunction {

  final case class BatchingResult(
    batches: List[(String, List[Request])],
    zones: Map[String, List[EdgeId]]
  )

}

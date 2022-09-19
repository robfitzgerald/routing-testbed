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
import scala.util.Random
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

case class RandomBatching(
  batchWindow: SimTime,
  maxBatchSize: Int
) extends BatchingFunction {

  val rng = new Random

  /**
    * takes the current batching strategy and any updates about replan-able agents, and spits out an
    * update to that batching strategy
    *
    * @param roadNetwork          the current road network state
    * @param activeRouteRequests agents which are available for SO routing requests
    * @param currentTime          the current sim time
    * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
    */
  def updateBatchingStrategy(
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    activeRouteRequests: List[RouteRequestData],
    currentTime: SimTime
  ): IO[Option[BatchingFunction.BatchingResult]] = {
    if (activeRouteRequests.isEmpty) IO.pure {
      None
    }
    else
      IO.pure {

        activeRouteRequests match {
          case Nil         => None
          case newRequests =>
            // we have agents that we can replan to add to the nearest possible request time
            val shuffled = rng.shuffle(newRequests)
            val batches: List[(String, List[Request])] = BatchSplittingFunction
              .bySlidingWindow(shuffled, this.maxBatchSize)
              .zipWithIndex
              .map { case (reqs, id) => (id.toString, reqs.map { _.request }) }
            val table  = batches.map { case (batchId, reqs) => batchId -> reqs.map { _.location }.toList }.toMap
            val result = BatchingFunction.BatchingResult(batches, table)
            Some(result)
        }
      }
  }
}

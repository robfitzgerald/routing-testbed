package edu.colorado.fitzgero.sotestbed.algorithm.batching

import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.LBTCAlgorithm.ClusterId
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{RoadNetwork, VertexId}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.{
  LocalAdjacencyGraphDualNetwork,
  LocalAdjacencyListFlowNetwork
}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

case class LBTCTrajectoryClusterBatching(
  omegaDelta: Double,
  omegaBeta: Double,
  omegaA: Double,
  omegaS: Double,
  maxIterations: Int,
  maxRuntimeMilliseconds: Int,
  trajectoryTimeLimit: SimTime
) extends BatchingFunction
    with LazyLogging {

  /**
    * takes the current batching strategy and any updates about replan-able agents, and spits out an
    * update to that batching strategy
    *
    * @param roadNetwork         the current road network state
    * @param activeRouteRequests agents which are available for SO routing requests
    * @param currentTime         the current sim time
    * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
    */
  def updateBatchingStrategy(
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    activeRouteRequests: List[AgentBatchData.RouteRequestData],
    currentTime: SimTime
  ): IO[Option[BatchingFunction.BatchingResult]] = {
    roadNetwork match {
      case graph: LocalAdjacencyListFlowNetwork =>
        // find batching strategy using trajectory clustering algorithm
        val graphDual    = LocalAdjacencyGraphDualNetwork(graph)
        val trajectories = LBTCAlgorithm.buildAgentTrajectoryLookup(activeRouteRequests, trajectoryTimeLimit)
        val algorithm    = LBTCAlgorithm(omegaDelta, omegaBeta, omegaA, omegaS)
        val clusterIdsByDualVertexId =
          algorithm.labelClustering(graphDual, trajectories, maxIterations, maxRuntimeMilliseconds)

        // convert algorithm result into batching strategy output
        val agentLocations = LBTCAlgorithm.agentLocations(activeRouteRequests)
        val requestsByClusterId: Map[Request, ClusterId] = for {
          (request, currentEdgeId) <- agentLocations
          clusterId                <- clusterIdsByDualVertexId.get(VertexId(currentEdgeId.value))
        } yield {
          (request, clusterId)
        }
        val batches = requestsByClusterId
          .groupBy { case (_, clusterId) => clusterId }
          .map {
            case (clusterId, reqsAndClusterIds) =>
              clusterId.toString -> reqsAndClusterIds.map { case (req, _) => req }.toList
          }
          .toList

        val edgesByClusterId = for {
          edgeId    <- graph.edgeIds
          clusterId <- clusterIdsByDualVertexId.get(VertexId(edgeId.value))
        } yield (clusterId.toString, edgeId)

        val edgeLookup = edgesByClusterId.foldLeft(Map.empty[String, List[EdgeId]]) {
          case (acc, (batchId, edgeId)) =>
            val prev = acc.getOrElse(batchId, List.empty)
            val next = edgeId +: prev
            acc.updated(batchId, next)
        }

        val avgClusterSize =
          if (batches.isEmpty) 0.0
          else batches.map { case (_, reqs) => reqs.size }.sum / batches.size
        logger.info(f"found ${batches.size} request clusters with avg cluster size $avgClusterSize%.2f")

        val lookup = (edgeId: EdgeId) => Option.empty
        val result = BatchingFunction.BatchingResult(batches, edgeLookup)
        IO.pure(Some(result))
      case _ =>
        IO.raiseError(new NotImplementedError(s"can only run this on a LocalAdjacencyListFlowNetwork"))
    }
  }
}

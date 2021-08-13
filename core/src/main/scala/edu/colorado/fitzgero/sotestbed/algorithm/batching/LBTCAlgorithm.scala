package edu.colorado.fitzgero.sotestbed.algorithm.batching

import java.io.PrintWriter

import scala.annotation.tailrec

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, VertexId}
import edu.colorado.fitzgero.sotestbed.algorithm.batching.GraphDualClusteringOps._
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyGraphDualNetwork.DualEdge
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.{LBTCValidationOps, LocalAdjacencyGraphDualNetwork}

case class LBTCAlgorithm(
  omegaDelta: Double,
  omegaBeta: Double,
  omegaA: Double,
  omegaS: Double
) extends LazyLogging {

  import LBTCAlgorithm._

  /**
    * inner operation of LBTC which uses the similarity of two vertices to determine
    * label propagation.
    *
    * @param vi the vertex to update
    * @param g provides graph operations for algorithm
    * @param a provides agent operations for algorithm
    * @return the neighbor with the label to update vi with
    */
  def chooseLabelToUpdate(
    graph: LocalAdjacencyGraphDualNetwork,
    trajectories: Map[VertexId, Set[String]],
    vi: VertexId
  ): VertexId = {
    graph.neighborNodes(vi).toList match {
      case Nil =>
        throw new IllegalArgumentException("network should be fully connected")
      case neighbors =>
        val (maxNeighbor, _) =
          neighbors
            .map { vx =>
              val s = similarity(graph, trajectories, vi, vx, omegaDelta, omegaBeta, omegaA, omegaS)
              (vx, s)
            }
            .maxBy { case (_, similarity) => similarity }
        maxNeighbor
    }

  }

  final case class LabelClusteringIteration(
    labels: Map[VertexId, ClusterId],
    labelsModifiedThisIteration: Boolean
  )

  /**
    * finds a clustering via the LBTC algorithm
    * @param maxIterations hard cap on iterations, hopefully never reached
    * @param g provides graph operations for algorithm
    * @param a provides agent operations for algorithm
    * @return a mapping from VertexId to ClusterId
    */
  def labelClustering(
    graph: LocalAdjacencyGraphDualNetwork,
    trajectories: Map[VertexId, Set[String]],
    maxIterations: Int = 100,
    maxRuntimeMilliseconds: Int
  ): Map[VertexId, ClusterId] = {

    val verticesDescByDensity =
      graph.verticesMap.keys.toList
        .map { vertexId => (vertexId, trajectories.density(vertexId)) }
        .sortBy { case (_, density) => density }
        .map { case (vertexId, _) => vertexId }

    val startTime = System.currentTimeMillis
    val endTime   = startTime + maxRuntimeMilliseconds

    @tailrec
    def _find_clusters(
      labels: Map[VertexId, ClusterId],
      prevDbi: Double = 0.0,
      iterations: Int = 0
    ): (Map[VertexId, ClusterId], Double, Int) = {
      if (iterations == maxIterations || endTime < System.currentTimeMillis) {
        (labels, prevDbi, iterations)
      } else {
        // for each vertex, choose it's label for update (sorted by density)
        val initial = LabelClusteringIteration(labels, labelsModifiedThisIteration = false)
        val propagationResult = verticesDescByDensity.foldLeft(initial) { (acc, vertexId) =>
          // algorithm 1 of LBTC
          val vertexIdToPropagateFrom = this.chooseLabelToUpdate(graph, trajectories, vertexId)

          // algorithm 2 of LBTC - inner loop
          val updateResult = for {
            prevClusterId   <- labels.get(vertexId)
            updateClusterId <- labels.get(vertexIdToPropagateFrom)
            if prevClusterId != updateClusterId
          } yield {

            // our label should change
            val updatedLabels = acc.labels.updated(vertexId, updateClusterId)
            val updatedAcc = acc.copy(
              labels = updatedLabels,
              labelsModifiedThisIteration = true
            )
            updatedAcc
          }
          // on account of failure in inner loop, we ignore (perhaps something to fix)
          updateResult.getOrElse(acc)
        }

        if (propagationResult.labelsModifiedThisIteration) {

          // evaluate the clustering
          val clusters: Map[ClusterId, List[DualEdge]] = clusterRepresentation(propagationResult.labels, graph)
          val thisDbi                                  = LBTCValidationOps.daviesBouldinIndex(clusters, graph)

          logger.whenDebugEnabled {
            val nClusters      = clusters.size
            val avgClusterSize = clusters.map { case (_, vertices) => vertices.length }.sum / nClusters
            logger.debug(
              f"iteration: $iterations - DBI: $thisDbi%.1f - clusters: $nClusters - avgClusterSize: $avgClusterSize"
            )
          }

          val noChangeToDbi = math.floor(thisDbi).toInt == math.floor(prevDbi).toInt
          if (noChangeToDbi) {
            // short-circuit this loop
            (propagationResult.labels, thisDbi, iterations)
          } else {
            // recurse
            _find_clusters(propagationResult.labels, thisDbi, iterations + 1)
          }
        } else {
          (labels, prevDbi, iterations)
        }
      }
    }
    val (result, dbi, iterations) = _find_clusters(verticesDescByDensity.zipWithIndex.toMap)
    val runTime                   = System.currentTimeMillis - startTime
    val runTimeSeconds            = f"${runTime.toDouble / 1000.0}%.2f"

    val clusters = clusterRepresentation(result, graph)

    logger.info(f"LBTC ran $iterations iterations in $runTimeSeconds seconds with final DBI $dbi%.1f")
    logger.whenDebugEnabled {
      val nonSingletonClusters =
        clusters.filter { case (_, edges) => edges.lengthCompare(1) > 0 }
      val nonSingletonDBI = LBTCValidationOps.daviesBouldinIndex(nonSingletonClusters, graph)
      logger.debug(
        f"${nonSingletonClusters.size} non-singleton graph edge clusters with DBI $nonSingletonDBI%.1f and avg cluster size ${nonSingletonClusters.map { _._2.size }.sum.toDouble / nonSingletonClusters.size}%.2f"
      )
    }

//    if (SimTime.hour(8) < currentTime) {
//      val pw: PrintWriter = new PrintWriter("clusters.csv")
//      pw.write("cluster,WKT\n")
//      for {
//        (cId, dualEdges) <- clusters
//        dualEdge         <- dualEdges
//      } {
//        pw.write(s"""$cId,"${dualEdge.toWkt()}"\n""")
//      }
//      pw.close()
//    }

    result
  }
}

object LBTCAlgorithm {
  type ClusterId = Int

  /**
    * used in [[buildAgentTrajectoryLookup]] function to find all
    * recent edges traversed by an agent
    *
    * @param trajectory
    * @param time
    * @param isDone
    */
  final case class TrajectoryBuilder(
    trajectory: List[VertexId] = List.empty[VertexId],
    time: SimTime = SimTime.Zero,
    isDone: Boolean = false
  )

  /**
    * builds a trajectory lookup table used in LBTC
    * @param activeRouteRequests the set of active agent requests
    * @param trajectoryTimeLimit read link traversals as trajectories this far into the past
    * @return the agent trajectory lookup table
    */
  def buildAgentTrajectoryLookup(
    activeRouteRequests: List[AgentBatchData.RouteRequestData],
    trajectoryTimeLimit: SimTime
  ): Map[VertexId, Set[String]] = {
    val agentLocations = activeRouteRequests.foldLeft(Map.empty[VertexId, Set[String]]) { (agentLocationsBuilder, r) =>
      val agent = r.request.agent
      val trajectoryBuilder: TrajectoryBuilder = r.experiencedRoute.reverse.foldLeft(TrajectoryBuilder()) {
        (trajectoryBuilder, edgeData) =>
          if (trajectoryBuilder.isDone) {
            trajectoryBuilder
          } else {
            val timeAtEdge   = edgeData.estimatedTimeAtEdge.getOrElse(SimTime.Zero)
            val withThisTime = trajectoryBuilder.time + timeAtEdge
            if (withThisTime > trajectoryTimeLimit) {
              trajectoryBuilder.copy(isDone = true)
            } else {
              val asVertexId = VertexId(edgeData.edgeId.value)
              trajectoryBuilder.copy(
                trajectory = asVertexId +: trajectoryBuilder.trajectory,
                time = withThisTime
              )
            }
          }
      }

      val result = trajectoryBuilder.trajectory.foldLeft(agentLocationsBuilder) { (alb, vertexId) =>
        val updatedAgents = alb.getOrElse(vertexId, Set.empty) + agent
        alb.updated(vertexId, updatedAgents)
      }

      result
    }
    agentLocations
  }

  /**
    * the current set of requests and their most recent EdgeId traversed
    * @param activeRouteRequests the active set of route requests
    * @return a mapping from Request to it's current EdgeId
    */
  def agentLocations(activeRouteRequests: List[AgentBatchData.RouteRequestData]): Map[Request, EdgeId] = {
    activeRouteRequests.flatMap { data =>
      data.experiencedRoute.lastOption.map { currentEdgeData => data.request -> currentEdgeData.edgeId }
    }.toMap
  }

  /**
    * swaps map orientation to place clusterIds as keys, and, inserts DualEdges as values
    * @param labels a labelling
    * @param graph the dual graph
    * @return labels in cluster representation
    */
  def clusterRepresentation(
    labels: Map[VertexId, ClusterId],
    graph: LocalAdjacencyGraphDualNetwork
  ): Map[ClusterId, List[DualEdge]] = {
    labels
      .groupBy { case (_, clusterId) => clusterId }
      .map { case (k, v) => k -> v.keys.toList.flatMap { graph.verticesMap.get } }
  }
}

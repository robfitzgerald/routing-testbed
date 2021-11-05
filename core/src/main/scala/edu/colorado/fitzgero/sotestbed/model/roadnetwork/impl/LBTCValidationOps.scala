package edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl

import edu.colorado.fitzgero.sotestbed.algorithm.batching.LBTCAlgorithm.ClusterId
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.VertexId
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyGraphDualNetwork.DualEdge
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

object LBTCValidationOps {

  def euclideanNorm(a: Coordinate, b: Coordinate): Double = {
    val xDiff = a.x - b.x
    val yDiff = a.y - b.y
    val xNorm = math.pow(xDiff, 2)
    val yNorm = math.pow(yDiff, 2)
    math.sqrt(xNorm + yNorm)
  }

  def clusterCentroid(clusterEdges: List[DualEdge]): Coordinate = {
    if (clusterEdges.isEmpty) throw new Error("empty cluster")
    else {
      val n = clusterEdges.length
      val (xs, ys) = clusterEdges.foldLeft((0.0, 0.0)) {
        case ((x, y), edge) =>
          (x + edge.centroid.x, y + edge.centroid.y)
      }
      val clusterCentroid = Coordinate(xs / n, ys / n)
      clusterCentroid
    }
  }

  /**
    * computes the compactness of a cluster (eq. 5 in LBTC paper)
    * @param clusterEdges the edges of a cluster
    * @return the compactness of the cluster
    */
  def compactness(clusterEdges: List[DualEdge], clusterCentroid: Coordinate): Double = {
    if (clusterEdges.isEmpty) 0.0
    else {
      val n          = clusterEdges.length
      val sumOfNorms = clusterEdges.map { edge => euclideanNorm(edge.centroid, clusterCentroid) }.sum
      val result     = sumOfNorms / n
      result
    }
  }

  /**
    * computes the Davies-Bouldin index for measuring the quality of a clustering
    * @param labels the labels of an iteration of LBTC
    * @param graph the road network graph
    * @return the DBI
    */
  def daviesBouldinIndex(clusters: Map[ClusterId, List[DualEdge]], graph: LocalAdjacencyGraphDualNetwork): Double = {

    val clusterData = for {
      (clusterId, cluster) <- clusters
      centroid = clusterCentroid(cluster)
    } yield {
      (clusterId, compactness(cluster, centroid), centroid)
    }

    val dbiTerms = for {
      (_, compactness, centroid) <- clusterData
    } yield {
      // for all clusters (including this one), find the maximum term
      // as described in eq. 6 in the LBTC paper
      val maxTerm = clusterData.foldLeft(Double.NegativeInfinity) {
        case (maxTerm, (_, compactnessJ, centroidJ)) =>
          val numer    = compactness + compactnessJ
          val denom    = euclideanNorm(centroid, centroidJ)
          val thisTerm = if (denom == 0.0) 0.0 else numer / denom
          math.max(maxTerm, thisTerm)
      }
      maxTerm
    }

    val result = dbiTerms.sum / clusters.size
    result
  }
}

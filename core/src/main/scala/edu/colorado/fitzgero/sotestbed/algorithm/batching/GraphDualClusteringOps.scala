package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.VertexId
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyGraphDualNetwork

object GraphDualClusteringOps {

  /**
    * methods used by LBTC Clustering
    * see Niu, Xinzheng, et al. "Label-based trajectory clustering in complex road networks."
    *  IEEE Transactions on Intelligent Transportation Systems 21.10 (2019): 4098-4110.
    *
    * @param graph graph dual of a road network
    */
  implicit class GraphDualClusteringOpsInstance(graph: LocalAdjacencyGraphDualNetwork) {

    def neighborNodes(v: VertexId): Set[VertexId] =
      graph.adjList.getOrElse(v, List.empty).toSet.union(graph.revAdjList.getOrElse(v, List.empty).toSet)

    def neighborNodeDegree(v: VertexId): Int = this.neighborNodes(v).size

    def speed(v: VertexId): Double =
      graph.verticesMap
        .get(v)
        .map { _.edgeBPR.freeFlowSpeed.value }
        .getOrElse(throw new Error(s"undefined speed on link $v"))

    /**
      * SS(v_i, v_j) Salton Index aka cosine similarity
      *
      * @param vi
      * @param vj
      * @return
      */
    def cosineSimilarity(vi: VertexId, vj: VertexId): Double = {
      val adjV1         = neighborNodes(vi)
      val adjV1Cnt      = adjV1.size.toDouble
      val adjV2         = neighborNodes(vj)
      val adjV2Cnt      = adjV2.size.toDouble
      val numer         = adjV1.intersect(adjV2).size.toDouble
      val denom: Double = math.sqrt(adjV1Cnt * adjV2Cnt)
      val result        = if (denom == 0.0) 0.0 else numer / denom
      result
    }

    /**
      * beta(v_i, v_j) speed limit factor
      *
      * @param vi
      * @param vj
      * @return
      */
    def speedFactor(vi: VertexId, vj: VertexId): Double = {
      val numer  = this.speed(vj)
      val denom  = this.neighborNodes(vi).map { this.speed }.foldLeft(0.0) { _ + _ }
      val result = if (denom == 0.0) 0.0 else numer / denom
      result
    }

  }

  implicit class AgentLocationOpsInstance(agentLocations: Map[VertexId, Set[String]]) {

    def flow(vi: VertexId, vj: VertexId): Double = {
      val result = for {
        agentsV1 <- this.agentLocations.get(vi)
        agentsV2 <- this.agentLocations.get(vj)
      } yield agentsV1.intersect(agentsV2).size
      result.getOrElse(0).toDouble
    }

    def density(v: VertexId): Double = this.agentLocations.getOrElse(v, Set.empty).size.toDouble

    /**
      * delta(v_i, v_j) flow factor
      *
      * @param vi
      * @param vj
      * @return
      */
    def flowFactor(vi: VertexId, vj: VertexId): Double = {
      val flow    = this.flow(vi, vj)
      val density = this.density(vi)
      if (density == 0.0) 0.0 else flow / density
    }
  }

  /**
    * AS(v_i, v_j)
    * @param vi
    * @param vj
    * @param omegaDelta
    * @param omegaBeta
    * @param g
    * @param a
    * @return
    */
  def attributeSimilarity(
    graph: LocalAdjacencyGraphDualNetwork,
    trajectories: Map[VertexId, Set[String]],
    vi: VertexId,
    vj: VertexId,
    omegaDelta: Double,
    omegaBeta: Double
  ): Double = {
    val deltaTerm = omegaDelta * trajectories.flowFactor(vi, vj)
    val betaTerm  = omegaBeta * graph.speedFactor(vi, vj)
    val result    = deltaTerm + betaTerm
    result
  }

  /**
    * F(v_i, v_j) similarity measure
    *
    * @param vi
    * @param vj
    * @param omegaDelta
    * @param omegaBeta
    * @param omegaA
    * @param omegaS
    * @return
    */
  def similarity(
    graph: LocalAdjacencyGraphDualNetwork,
    trajectories: Map[VertexId, Set[String]],
    vi: VertexId,
    vj: VertexId,
    omegaDelta: Double,
    omegaBeta: Double,
    omegaA: Double,
    omegaS: Double
  ): Double = {
    val aTerm  = omegaA * attributeSimilarity(graph, trajectories, vi, vj, omegaDelta, omegaBeta)
    val sTerm  = omegaS * graph.cosineSimilarity(vi, vj)
    val result = aTerm + sTerm
    result
  }
}

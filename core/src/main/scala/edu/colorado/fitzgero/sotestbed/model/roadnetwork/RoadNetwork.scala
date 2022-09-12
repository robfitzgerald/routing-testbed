package edu.colorado.fitzgero.sotestbed.model.roadnetwork

import edu.colorado.fitzgero.sotestbed.model.numeric.Flow
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork.EdgeTriplet
import edu.colorado.fitzgero.sotestbed.model.numeric.MetersPerSecond

trait RoadNetwork[F[_], V, E] {
  // topology
  def vertexIds: List[VertexId]
  def edgeIds: List[EdgeId]
  def vertex(vertexId: VertexId): F[Option[RoadNetwork.VertexIdAndAttribute[V]]]
  def vertices: F[List[RoadNetwork.VertexIdAndAttribute[V]]]
  def vertices(vertexIds: List[VertexId]): F[List[RoadNetwork.VertexIdAndAttribute[V]]]
  def edge(edgeId: EdgeId): F[Option[RoadNetwork.EdgeIdAndAttribute[E]]]
  def edgeTriplets: F[List[RoadNetwork.EdgeTriplet[E]]]
  def edges(edgeIds: List[EdgeId]): F[List[RoadNetwork.EdgeIdAndAttribute[E]]]
  def hasVertex(vertexId: VertexId): F[Boolean]
  def hasEdge(edgeId: EdgeId): F[Boolean]

  // relationships
  def source(edgeId: EdgeId): F[Option[VertexId]]
  def destination(edgeId: EdgeId): F[Option[VertexId]]
  def incidentEdges(vertexId: VertexId, direction: TraverseDirection): F[List[EdgeId]]
  def neighbors(vertexId: VertexId, direction: TraverseDirection): F[List[VertexId]]
  def incidentEdgeTriplets(vertexId: VertexId, direction: TraverseDirection): F[List[EdgeTriplet[E]]]

  // paths

  // data structure
  def updateEdgeFlows(
    flows: List[(EdgeId, Option[Flow], MetersPerSecond)],
    updateFn: (E, Option[Flow], MetersPerSecond) => E
  ): F[RoadNetwork[F, V, E]]
}

object RoadNetwork {

  final case class VertexIdAndAttribute[V](
    vertexId: VertexId,
    attribute: V
  )

  final case class EdgeIdAndAttribute[E](
    edgeId: EdgeId,
    attribute: E
  )

  final case class EdgeTriplet[E](src: VertexId, edgeId: EdgeId, dst: VertexId, attr: E) {
    override def toString: String = s"EdgeTriplet(${src.value} -[${edgeId.value}]-> ${dst.value})"
  }
}

package edu.colorado.fitzgero.sotestbed.model.roadnetwork

import edu.colorado.fitzgero.sotestbed.model.numeric.Flow

trait RoadNetwork [F[_], V, E] {
  // topology
  def vertex(vertexId: VertexId): F[Option[V]]
  def vertices(vertexIds: List[VertexId]): F[List[V]]
  def edge(edgeId: EdgeId): F[Option[E]]
  def edges(edgeIds: List[EdgeId]): F[List[E]]
  def hasVertex(vertexId: VertexId): F[Boolean]
  def hasEdge(edgeId: EdgeId): F[Boolean]

  // relationships
  def source(edgeId: EdgeId): F[Option[VertexId]]
  def destination(edgeId: EdgeId): F[Option[VertexId]]
  def incidentEdges(vertexId: VertexId, direction: TraverseDirection): F[List[EdgeId]]
  def neighbors(vertexId: VertexId, direction: TraverseDirection): F[List[VertexId]]
  def edgesAndNeighbors(vertexId: VertexId, direction: TraverseDirection): F[List[(EdgeId, VertexId)]]

  // data structure
  def updateEdgeFlows(flows: List[(EdgeId, Flow)], edgeUpdateFunction: (E, Flow) => E): F[RoadNetwork[F, V, E]]
}

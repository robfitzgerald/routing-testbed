package edu.colorado.fitzgero.sotestbed.model.roadnetwork

trait RoadNetworkState[V, E] {

  // topology
  def vertex(vertexId: VertexId): Option[V]
  def vertices(vertexIds: List[VertexId]): List[V]
  def edge(edgeId: EdgeId): Option[E]
  def edges(edgeIds: List[EdgeId]): List[E]
  def hasVertex(vertexId: VertexId): Boolean
  def hasEdge(edgeId: EdgeId): Boolean

  // relationships
  def source(edgeId: EdgeId): Option[VertexId]
  def destination(edgeId: EdgeId): Option[VertexId]
  def incidentEdges(vertexId: VertexId, direction: TraverseDirection): List[EdgeId]
  def neighbors(vertexId: VertexId, direction: TraverseDirection): List[VertexId]
  def edgesAndNeighbors(vertexId: VertexId, direction: TraverseDirection): List[(EdgeId, VertexId)]

  // data structure -> push into a typeclass?
//  def getRoadNetwork: RoadNetworkModel[F, V, E]
}

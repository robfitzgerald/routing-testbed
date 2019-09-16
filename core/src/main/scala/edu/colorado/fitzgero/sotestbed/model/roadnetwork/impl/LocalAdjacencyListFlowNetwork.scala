package edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.model.numeric.Flow
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{
  EdgeId,
  RoadNetworkMutable,
  RoadNetworkState,
  TraverseDirection,
  VertexId
}

class LocalAdjacencyListFlowNetwork[V, E](
    private var edges: Map[EdgeId, LocalAdjacencyListFlowNetwork.EdgeTriplet[E]],
    val vertices: Map[VertexId, V],
    val adjList: Map[VertexId, Map[EdgeId, VertexId]],
    val revAdjList: Map[VertexId, Map[EdgeId, VertexId]]
) extends RoadNetworkState[V, E]
    with RoadNetworkMutable[IO, E] {
  def vertex(vertexId: VertexId): Option[V] = vertices.get(vertexId)

  def vertices(vertexIds: List[VertexId]): List[V] = vertexIds.flatMap { vertices.get }

  def edge(edgeId: EdgeId): Option[E] = edges.get(edgeId).map { _.edge }

  def edges(edgeIds: List[EdgeId]): List[E] = edgeIds.flatMap { edges.get(_).map { _.edge } }

  def hasVertex(vertexId: VertexId): Boolean = vertices.isDefinedAt(vertexId)

  def hasEdge(edgeId: EdgeId): Boolean = edges.isDefinedAt(edgeId)

  def source(edgeId: EdgeId): Option[VertexId] = edges.get(edgeId).map { _.src }

  def destination(edgeId: EdgeId): Option[VertexId] = edges.get(edgeId).map { _.dst }

  def incidentEdges(vertexId: VertexId, direction: TraverseDirection): List[EdgeId] =
    direction match {
      case TraverseDirection.Forward => adjList.get(vertexId).toList.flatMap { _.keys }
      case TraverseDirection.Reverse => revAdjList.get(vertexId).toList.flatMap { _.keys }
    }

  def neighbors(vertexId: VertexId, direction: TraverseDirection): List[VertexId] =
    direction match {
      case TraverseDirection.Forward => adjList.get(vertexId).toList.flatMap { _.values }
      case TraverseDirection.Reverse => revAdjList.get(vertexId).toList.flatMap { _.values }
    }

  def edgesAndNeighbors(vertexId: VertexId,
                        direction: TraverseDirection): List[(EdgeId, VertexId)] =
    direction match {
      case TraverseDirection.Forward => adjList.get(vertexId).toList.flatMap { _.toList }
      case TraverseDirection.Reverse => revAdjList.get(vertexId).toList.flatMap { _.toList }
    }

  def updateEdgeFlows(flows: List[(EdgeId, Flow)], edgeUpdateFunction: (E, Flow) => E): IO[Unit] =
    IO {
      {
        val updatedEdges =
          flows.foldLeft(edges) {
            case (acc, (edgeId, flow)) =>
              acc.get(edgeId) match {
                case None => acc
                case Some(edgeTriplet) =>
                  val updatedEdgeTriplet =
                    edgeTriplet.copy(edge = edgeUpdateFunction(edgeTriplet.edge, flow))
                  acc.updated(edgeId, updatedEdgeTriplet)
              }
          }
        edges = updatedEdges
      }
    }
}

object LocalAdjacencyListFlowNetwork {
  final case class EdgeTriplet[E](src: VertexId, dst: VertexId, edge: E)
}

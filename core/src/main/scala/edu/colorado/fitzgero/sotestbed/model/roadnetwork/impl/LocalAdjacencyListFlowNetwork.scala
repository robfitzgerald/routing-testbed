package edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.model.numeric.Flow
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{
  EdgeId,
  RoadNetwork,
  TraverseDirection,
  VertexId
}

case class LocalAdjacencyListFlowNetwork[V, E](
    edges: Map[EdgeId, LocalAdjacencyListFlowNetwork.EdgeTriplet[E]],
    vertices: Map[VertexId, V],
    adjList: Map[VertexId, Map[EdgeId, VertexId]],
    revAdjList: Map[VertexId, Map[EdgeId, VertexId]]
) extends RoadNetwork[IO, V, E] {

  def vertex(vertexId: VertexId): IO[Option[V]] = IO { vertices.get(vertexId) }

  def vertices(vertexIds: List[VertexId]): IO[List[V]] = IO { vertexIds.flatMap { vertices.get } }

  def edge(edgeId: EdgeId): IO[Option[E]] = IO { edges.get(edgeId).map { _.edge } }

  def edges(edgeIds: List[EdgeId]): IO[List[E]] = IO { edgeIds.flatMap { edges.get(_).map { _.edge } } }

  def hasVertex(vertexId: VertexId): IO[Boolean] = IO { vertices.isDefinedAt(vertexId) }

  def hasEdge(edgeId: EdgeId): IO[Boolean] = IO { edges.isDefinedAt(edgeId) }

  def source(edgeId: EdgeId): IO[Option[VertexId]] = IO { edges.get(edgeId).map { _.src } }

  def destination(edgeId: EdgeId): IO[Option[VertexId]] = IO { edges.get(edgeId).map { _.dst } }

  def incidentEdges(vertexId: VertexId, direction: TraverseDirection): IO[List[EdgeId]] = IO {
    direction match {
      case TraverseDirection.Forward => adjList.get(vertexId).toList.flatMap { _.keys }
      case TraverseDirection.Reverse => revAdjList.get(vertexId).toList.flatMap { _.keys }
    }
  }

  def neighbors(vertexId: VertexId, direction: TraverseDirection): IO[List[VertexId]] = IO {
    direction match {
      case TraverseDirection.Forward => adjList.get(vertexId).toList.flatMap { _.values }
      case TraverseDirection.Reverse => revAdjList.get(vertexId).toList.flatMap { _.values }
    }
  }

  def edgesAndNeighbors(vertexId: VertexId,
                        direction: TraverseDirection): IO[List[(EdgeId, VertexId)]] = IO {
    direction match {
      case TraverseDirection.Forward => adjList.get(vertexId).toList.flatMap { _.toList }
      case TraverseDirection.Reverse => revAdjList.get(vertexId).toList.flatMap { _.toList }
    }
  }

  def updateEdgeFlows(flows: List[(EdgeId, Flow)], edgeUpdateFunction: (E, Flow) => E): IO[LocalAdjacencyListFlowNetwork[V, E]] =
    IO {

      // update only the edges provided in the flows argument
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

      this.copy(
        edges = updatedEdges
      )
    }
}

object LocalAdjacencyListFlowNetwork {
  final case class EdgeTriplet[E](src: VertexId, dst: VertexId, edge: E)
}

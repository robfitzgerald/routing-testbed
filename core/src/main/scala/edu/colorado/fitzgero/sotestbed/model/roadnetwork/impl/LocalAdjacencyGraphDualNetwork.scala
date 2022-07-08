package edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.model.numeric.Flow
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetwork, TraverseDirection, VertexId}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

final case class LocalAdjacencyGraphDualNetwork(
  verticesMap: Map[VertexId, LocalAdjacencyGraphDualNetwork.DualEdge],
  adjList: Map[VertexId, List[VertexId]],
  revAdjList: Map[VertexId, List[VertexId]]
) extends RoadNetwork[IO, LocalAdjacencyGraphDualNetwork.DualEdge, Unit] {

  import LocalAdjacencyGraphDualNetwork._

  def vertexIds: List[VertexId] = ???

  def edgeIds: List[EdgeId] = ???

  def vertex(
    vertexId: VertexId
  ): IO[Option[RoadNetwork.VertexIdAndAttribute[DualEdge]]] =
    IO.pure {
      verticesMap.get(vertexId).map { attr => RoadNetwork.VertexIdAndAttribute(vertexId, attr) }
    }

  def vertices: IO[List[RoadNetwork.VertexIdAndAttribute[DualEdge]]] =
    IO.pure {
      verticesMap.map { case (vertexId, attr) => RoadNetwork.VertexIdAndAttribute(vertexId, attr) }.toList
    }

  def vertices(
    vertexIds: List[VertexId]
  ): IO[List[RoadNetwork.VertexIdAndAttribute[DualEdge]]] =
    IO.pure {
      for {
        vertexId <- vertexIds
        attr     <- verticesMap.get(vertexId)
      } yield {
        RoadNetwork.VertexIdAndAttribute(vertexId, attr)
      }
    }

  def edge(edgeId: EdgeId): IO[Option[RoadNetwork.EdgeIdAndAttribute[Unit]]] =
    IO.raiseError(new Error("graph dual has no edge ids, edge attributes"))

  def edgeTriplets: IO[List[RoadNetwork.EdgeTriplet[Unit]]] =
    IO.raiseError(new Error("graph dual has no edge ids, edge attributes"))

  def edges(edgeIds: List[EdgeId]): IO[List[RoadNetwork.EdgeIdAndAttribute[Unit]]] =
    IO.raiseError(new Error("graph dual has no edge ids, edge attributes"))

  def hasVertex(vertexId: VertexId): IO[Boolean] = IO.pure { verticesMap.isDefinedAt(vertexId) }

  def hasEdge(edgeId: EdgeId): IO[Boolean] = IO.raiseError(new Error("graph dual has no edge ids, edge attributes"))

  def source(edgeId: EdgeId): IO[Option[VertexId]] =
    IO.raiseError(new Error("graph dual has no edge ids, edge attributes"))

  def destination(edgeId: EdgeId): IO[Option[VertexId]] =
    IO.raiseError(new Error("graph dual has no edge ids, edge attributes"))

  def incidentEdges(vertexId: VertexId, direction: TraverseDirection): IO[List[EdgeId]] =
    IO.raiseError(new Error("graph dual has no edge ids, edge attributes"))

  def neighbors(vertexId: VertexId, direction: TraverseDirection): IO[List[VertexId]] =
    IO.pure {
      direction match {
        case TraverseDirection.Forward => adjList.getOrElse(vertexId, List.empty)
        case TraverseDirection.Reverse => revAdjList.getOrElse(vertexId, List.empty)
      }
    }

  def incidentEdgeTriplets(vertexId: VertexId, direction: TraverseDirection): IO[List[RoadNetwork.EdgeTriplet[Unit]]] =
    IO.raiseError(new Error("graph dual has no edge ids, edge attributes"))

  def updateEdgeFlows(
    flows: List[(EdgeId, Flow)],
    edgeUpdateFunction: (Unit, Flow) => Unit
  ): IO[RoadNetwork[IO, DualEdge, Unit]] =
    IO.raiseError(new Error("graph dual has no edge ids, edge attributes, does not update"))
}

object LocalAdjacencyGraphDualNetwork {

  final case class DualEdge(edgeBPR: EdgeBPR, src: Coordinate, centroid: Coordinate, dst: Coordinate) {

    def toWkt(xyOrdering: Boolean = true): String =
      if (xyOrdering) s"LINESTRING (${src.x} ${src.y}, ${dst.x} ${dst.y})"
      else s"LINESTRING (${src.y} ${src.x}, ${dst.y} ${dst.x})"
  }

  /**
    * builds a graph dual, where the edges are now vertices and the new edges are
    * simple connections between edges in the original.
    * @param network flow network to build a graph dual from
    * @return a graph dual network
    */
  def apply(network: LocalAdjacencyListFlowNetwork): LocalAdjacencyGraphDualNetwork = {

    val verticesMap: Map[VertexId, DualEdge] = for {
      (edgeId, triplet) <- network.edgesMap
      src               <- network.verticesMap.get(triplet.src)
      dst               <- network.verticesMap.get(triplet.dst)
    } yield {
      val (midX, midY) = ((src.x + dst.x) / 2, (src.y + dst.y) / 2)
      val centroid     = Coordinate(midX, midY)
      VertexId(edgeId.value) -> DualEdge(triplet.attr, src, centroid, dst)
    }

    val adjList: Map[VertexId, List[VertexId]] = for {
      (edgeId, triplet) <- network.edgesMap
      neighborAdj       <- network.adjList.get(triplet.dst)
    } yield {
      VertexId(edgeId.value) -> neighborAdj.keys.toList.map { edgeId => VertexId(edgeId.value) }
    }

    val revAdjList: Map[VertexId, List[VertexId]] = for {
      (edgeId, triplet) <- network.edgesMap
      revNeighborAdj    <- network.revAdjList.get(triplet.src)
    } yield {
      VertexId(edgeId.value) -> revNeighborAdj.keys.toList.map { edgeId => VertexId(edgeId.value) }
    }

    LocalAdjacencyGraphDualNetwork(verticesMap, adjList, revAdjList)
  }
}

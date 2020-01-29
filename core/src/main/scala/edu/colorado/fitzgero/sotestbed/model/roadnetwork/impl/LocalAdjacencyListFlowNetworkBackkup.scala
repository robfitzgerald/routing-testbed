//package edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl
//
//import java.io.File
//
//import scala.util.Try
//import scala.xml.XML
//
//import cats.effect.SyncIO
//
//import cse.bdlab.fitzgero.sorouting.common.util.XMLParserIgnoresDTD
//import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, Meters, MetersPerSecond, NonNegativeNumber}
//import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
//import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetwork, TraverseDirection, VertexId}
//
//case class LocalAdjacencyListFlowNetwork[V, E](
//    edges: Map[EdgeId, RoadNetwork.EdgeTriplet[E]],
//    vertices: Map[VertexId, V],
//    adjList: Map[VertexId, Map[EdgeId, VertexId]],
//    revAdjList: Map[VertexId, Map[EdgeId, VertexId]]
//) extends RoadNetwork[SyncIO, V, E] {
//
//  def vertex(vertexId: VertexId): SyncIO[Option[RoadNetwork.VertexIdAndAttribute[V]]] = SyncIO {
//    vertices.get(vertexId).map { attr =>
//      RoadNetwork.VertexIdAndAttribute(vertexId, attr)
//    }
//  }
//
//  def vertices(vertexIds: List[VertexId]): SyncIO[List[RoadNetwork.VertexIdAndAttribute[V]]] = SyncIO {
//    for {
//      vertexId <- vertexIds
//      attr     <- vertices.get(vertexId)
//    } yield {
//      RoadNetwork.VertexIdAndAttribute(vertexId, attr)
//    }
//  }
//
//  def edge(edgeId: EdgeId): SyncIO[Option[RoadNetwork.EdgeIdAndAttribute[E]]] = SyncIO {
//    edges.get(edgeId).map {
//      case RoadNetwork.EdgeTriplet(_, _, _, attr) =>
//        RoadNetwork.EdgeIdAndAttribute(edgeId, attr)
//    }
//  }
//
//  def edges(edgeIds: List[EdgeId]): SyncIO[List[RoadNetwork.EdgeIdAndAttribute[E]]] = SyncIO {
//    for {
//      edgeId                                                <- edgeIds
//      RoadNetwork.EdgeTriplet(_, _, _, attr) <- edges.get(edgeId)
//    } yield {
//      RoadNetwork.EdgeIdAndAttribute(edgeId, attr)
//    }
//  }
//
//  def hasVertex(vertexId: VertexId): SyncIO[Boolean] = SyncIO { vertices.isDefinedAt(vertexId) }
//
//  def hasEdge(edgeId: EdgeId): SyncIO[Boolean] = SyncIO { edges.isDefinedAt(edgeId) }
//
//  def source(edgeId: EdgeId): SyncIO[Option[VertexId]] = SyncIO { edges.get(edgeId).map { _.src } }
//
//  def destination(edgeId: EdgeId): SyncIO[Option[VertexId]] = SyncIO { edges.get(edgeId).map { _.dst } }
//
//  def incidentEdges(vertexId: VertexId, direction: TraverseDirection): SyncIO[List[EdgeId]] = SyncIO {
//    direction match {
//      case TraverseDirection.Forward => adjList.get(vertexId).toList.flatMap { _.keys }
//      case TraverseDirection.Reverse => revAdjList.get(vertexId).toList.flatMap { _.keys }
//    }
//  }
//
//  def neighbors(vertexId: VertexId, direction: TraverseDirection): SyncIO[List[VertexId]] = SyncIO {
//    direction match {
//      case TraverseDirection.Forward => adjList.get(vertexId).toList.flatMap { _.values }
//      case TraverseDirection.Reverse => revAdjList.get(vertexId).toList.flatMap { _.values }
//    }
//  }
//
//  def incidentEdgeTriplets(vertexId: VertexId, direction: TraverseDirection): SyncIO[List[RoadNetwork.EdgeTriplet[E]]] = SyncIO {
//    val lookup: Map[VertexId, Map[EdgeId, VertexId]] = direction match {
//      case TraverseDirection.Forward => adjList
//      case TraverseDirection.Reverse => revAdjList
//    }
//    for {
//      neighbors <- lookup.get(vertexId).toList
//      edgeId <- neighbors.keys
//      edgeTriplet <- edges.get(edgeId).toList
//    } yield {
//      edgeTriplet
//    }
//  }
//
//  def updateEdgeFlows(flows: List[(EdgeId, Flow)], edgeUpdateFunction: (E, Flow) => E): SyncIO[LocalAdjacencyListFlowNetwork[V, E]] =
//    SyncIO {
//
//      val flowsMap: Map[EdgeId, Flow] = flows.toMap
//
//      // update all edges with marginal flows
//      val updatedEdges: Map[EdgeId, RoadNetwork.EdgeTriplet[E]] =
//        this.edges.keys.foldLeft(edges) {
//          case (acc, edgeId) =>
//            val marginalFlow: Flow = flowsMap.get(edgeId) match {
//              case None => Flow.Zero
//              case Some(flow) => flow
//            }
//            acc.get(edgeId) match {
//              case None => acc
//              case Some(edgeTriplet) =>
//                val updatedEdgeTriplet: RoadNetwork.EdgeTriplet[E] =
//                  edgeTriplet.copy(attr = edgeUpdateFunction(edgeTriplet.attr, marginalFlow))
//                acc.updated(edgeId, updatedEdgeTriplet)
//            }
//        }
//
//      this.copy(
//        edges = updatedEdges
//      )
//    }
//}

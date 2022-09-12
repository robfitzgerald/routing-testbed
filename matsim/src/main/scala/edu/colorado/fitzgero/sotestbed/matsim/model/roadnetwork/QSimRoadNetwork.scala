package edu.colorado.fitzgero.sotestbed.matsim.model.roadnetwork

import cats.effect.IO
import edu.colorado.fitzgero.sotestbed.model.numeric.Flow
import edu.colorado.fitzgero.sotestbed.model.roadnetwork._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.TraverseDirection.Forward
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.TraverseDirection.Reverse
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.network.Link
import org.matsim.api.core.v01.network.Node
import org.matsim.core.mobsim.qsim.QSim

import scala.collection.JavaConverters._
import edu.colorado.fitzgero.sotestbed.model.numeric.MetersPerSecond

final case class QSimRoadNetwork(qSim: QSim) extends RoadNetwork[IO, Node, Link] {

  // topology
  def vertexIds: List[VertexId] =
    qSim.getNetsimNetwork.getNetsimNodes.asScala.keys.toList.map { i => VertexId(i.toString) }

  def edgeIds: List[EdgeId] =
    qSim.getNetsimNetwork.getNetsimLinks.asScala.keys.toList.map { i => EdgeId(i.toString) }

  def vertex(vertexId: VertexId): IO[Option[RoadNetwork.VertexIdAndAttribute[Node]]] = {
    val nodeId = Id.createNodeId(vertexId.value)
    IO {
      qSim.getNetsimNetwork.getNetsimNodes.asScala
        .get(nodeId)
        .map { node => RoadNetwork.VertexIdAndAttribute(vertexId, node.getNode) }
    }
  }

  def vertices: IO[List[RoadNetwork.VertexIdAndAttribute[Node]]] = {
    IO {
      qSim.getNetsimNetwork.getNetsimNodes.asScala.toList.map {
        case (nodeId, node) =>
          RoadNetwork.VertexIdAndAttribute(VertexId(nodeId.toString), node.getNode)
      }
    }
  }

  def vertices(vertexIds: List[VertexId]): IO[List[RoadNetwork.VertexIdAndAttribute[Node]]] = {
    IO {
      val lookup = qSim.getNetsimNetwork.getNetsimNodes.asScala
      vertexIds.flatMap { id =>
        val nodeId = Id.createNodeId(id.value)
        lookup.get(nodeId).map { node => RoadNetwork.VertexIdAndAttribute(VertexId(nodeId.toString), node.getNode) }
      }
    }
  }

  def edge(edgeId: EdgeId): IO[Option[RoadNetwork.EdgeIdAndAttribute[Link]]] = {
    val linkId = Id.createLinkId(edgeId.value)
    IO {
      qSim.getNetsimNetwork.getNetsimLinks.asScala
        .get(linkId)
        .map { link => RoadNetwork.EdgeIdAndAttribute(edgeId, link.getLink) }
    }
  }

  def edgeTriplets: IO[List[RoadNetwork.EdgeTriplet[Link]]] = {
    IO {
      qSim.getNetsimNetwork.getNetsimLinks.asScala.toList
        .map {
          case (linkId, link) =>
            val src = VertexId(link.getLink.getFromNode.getId.toString)
            val dst = VertexId(link.getLink.getToNode.getId.toString)
            val id  = EdgeId(link.getLink.getId.toString)
            RoadNetwork.EdgeTriplet(src, id, dst, link.getLink)
        }
    }
  }

  def edges(edgeIds: List[EdgeId]): IO[List[RoadNetwork.EdgeIdAndAttribute[Link]]] = {
    IO {
      val lookup = qSim.getNetsimNetwork.getNetsimLinks.asScala
      edgeIds.flatMap { id =>
        val linkId = Id.createLinkId(id.value)
        lookup.get(linkId).map { link => RoadNetwork.EdgeIdAndAttribute(EdgeId(linkId.toString), link.getLink) }
      }
    }
  }

  def hasVertex(vertexId: VertexId): IO[Boolean] =
    IO { qSim.getNetsimNetwork.getNetsimNodes.asScala.isDefinedAt(Id.createNodeId(vertexId.value)) }

  def hasEdge(edgeId: EdgeId): IO[Boolean] =
    IO { qSim.getNetsimNetwork.getNetsimLinks.asScala.isDefinedAt(Id.createLinkId(edgeId.value)) }

  // relationships
  def source(edgeId: EdgeId): IO[Option[VertexId]] = {
    IO {
      val linkId = Id.createLinkId(edgeId.value)
      qSim.getNetsimNetwork.getNetsimLinks.asScala
        .get(linkId)
        .map { link => VertexId(link.getLink.getFromNode.getId.toString) }
    }
  }

  def destination(edgeId: EdgeId): IO[Option[VertexId]] = {
    IO {
      val linkId = Id.createLinkId(edgeId.value)
      qSim.getNetsimNetwork.getNetsimLinks.asScala
        .get(linkId)
        .map { link => VertexId(link.getLink.getToNode.getId.toString) }
    }
  }

  def incidentEdges(vertexId: VertexId, direction: TraverseDirection): IO[List[EdgeId]] = {
    IO {
      val nodeId = Id.createNodeId(vertexId.value)
      qSim.getNetsimNetwork.getNetsimNodes.asScala
        .get(nodeId)
        .map { node =>
          val incident = direction match {
            case Forward => node.getNode.getOutLinks
            case Reverse => node.getNode.getInLinks
          }
          incident.asScala.keys.toList.map { id => EdgeId(id.toString) }
        }
        .getOrElse(List.empty)
    }
  }

  def neighbors(vertexId: VertexId, direction: TraverseDirection): IO[List[VertexId]] = {
    incidentEdges(vertexId, direction).flatMap { edgeIds =>
      IO {
        edgeIds.flatMap { edgeId =>
          val id      = Id.createLinkId(edgeId.value)
          val linkOpt = qSim.getNetsimNetwork.getNetsimLinks.asScala.get(id)
          linkOpt.map { link =>
            direction match {
              case Forward => VertexId(link.getLink.getToNode.getId.toString)
              case Reverse => VertexId(link.getLink.getFromNode.getId.toString)
            }
          }
        }
      }
    }
  }

  def incidentEdgeTriplets(vertexId: VertexId, direction: TraverseDirection): IO[List[RoadNetwork.EdgeTriplet[Link]]] =
    incidentEdges(vertexId, direction).flatMap { edgeIds =>
      IO {
        edgeIds.map { edgeId =>
          val linkId = Id.createLinkId(edgeId.value)
          val link   = qSim.getNetsimNetwork.getNetsimLink(linkId)
          val src    = VertexId(link.getLink.getFromNode.getId.toString)
          val dst    = VertexId(link.getLink.getToNode.getId.toString)
          val id     = EdgeId(link.getLink.getId.toString)
          RoadNetwork.EdgeTriplet(src, id, dst, link.getLink)
        }
      }
    }

  // paths

  // data structure
  def updateEdgeFlows(
    flows: List[(EdgeId, Option[Flow], MetersPerSecond)],
    updateFn: (Link, Option[Flow], MetersPerSecond) => Link
  ): IO[RoadNetwork[IO, Node, Link]] = IO.raiseError(new Error("updateEdgeFlows not defined on QSimRoadNetwork"))
}

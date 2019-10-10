package edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl

import java.io.File

import scala.util.Try
import scala.xml.XML

import cats.effect.IO

import cse.bdlab.fitzgero.sorouting.common.util.XMLParserIgnoresDTD
import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, Meters, MetersPerSecond, NaturalNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetwork, TraverseDirection, VertexId}

case class LocalAdjacencyListFlowNetwork[V, E](
    edges: Map[EdgeId, RoadNetwork.EdgeTriplet[E]],
    vertices: Map[VertexId, V],
    adjList: Map[VertexId, Map[EdgeId, VertexId]],
    revAdjList: Map[VertexId, Map[EdgeId, VertexId]]
) extends RoadNetwork[IO, V, E] {

  def vertex(vertexId: VertexId): IO[Option[RoadNetwork.VertexIdAndAttribute[V]]] = IO {
    vertices.get(vertexId).map { attr =>
      RoadNetwork.VertexIdAndAttribute(vertexId, attr)
    }
  }

  def vertices(vertexIds: List[VertexId]): IO[List[RoadNetwork.VertexIdAndAttribute[V]]] = IO {
    for {
      vertexId <- vertexIds
      attr     <- vertices.get(vertexId)
    } yield {
      RoadNetwork.VertexIdAndAttribute(vertexId, attr)
    }
  }

  def edge(edgeId: EdgeId): IO[Option[RoadNetwork.EdgeIdAndAttribute[E]]] = IO {
    edges.get(edgeId).map {
      case RoadNetwork.EdgeTriplet(_, _, _, attr) =>
        RoadNetwork.EdgeIdAndAttribute(edgeId, attr)
    }
  }

  def edges(edgeIds: List[EdgeId]): IO[List[RoadNetwork.EdgeIdAndAttribute[E]]] = IO {
    for {
      edgeId                                                <- edgeIds
      RoadNetwork.EdgeTriplet(_, _, _, attr) <- edges.get(edgeId)
    } yield {
      RoadNetwork.EdgeIdAndAttribute(edgeId, attr)
    }
  }

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

  def incidentEdgeTriplets(vertexId: VertexId, direction: TraverseDirection): IO[List[RoadNetwork.EdgeTriplet[E]]] = IO {
    val lookup: Map[VertexId, Map[EdgeId, VertexId]] = direction match {
      case TraverseDirection.Forward => adjList
      case TraverseDirection.Reverse => revAdjList
    }
    for {
      neighbors <- lookup.get(vertexId).toList
      edgeId <- neighbors.keys
      edgeTriplet <- edges.get(edgeId).toList
    } yield {
      edgeTriplet
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
                  edgeTriplet.copy(attr = edgeUpdateFunction(edgeTriplet.attr, flow))
                acc.updated(edgeId, updatedEdgeTriplet)
            }
        }

      this.copy(
        edges = updatedEdges
      )
    }
}

object LocalAdjacencyListFlowNetwork {

  final case class Coordinate(x: Double, y: Double)

  final case class VerticesBuilder(
    vertices: Map[VertexId, Coordinate] = Map.empty,
    failedVertices: List[String] = List.empty
  )

  final case class EdgesBuilder(
    edges: Map[EdgeId, RoadNetwork.EdgeTriplet[EdgeBPR]] = Map.empty,
    adjList: Map[VertexId, Map[EdgeId, VertexId]] = Map.empty,
    revAdjList: Map[VertexId, Map[EdgeId, VertexId]] = Map.empty,
    failedEdges: List[String] = List.empty
  )

  val MaxPassengerVehicleLength: Meters = Meters(16.8)                    // meters
  val VehicleSpacer: Meters             = MaxPassengerVehicleLength * 1.5 // traffic jam vehicle spacing
  val DefaultFreeSpeed: Double          = 11                              // 25mph aka 11 meters per second
  val DefaultLanes: Double              = 1                               // at least 1 lane

  /**
    * loads a MATSim network from a file
    * @param networkFile the file location of a network
    * @return either a local adjacency graph representation of a MATSim network, or, reports on failures from parsing
    */
  final def fromMATSimXML(networkFile: File): Either[String, LocalAdjacencyListFlowNetwork[Coordinate, EdgeBPR]] = {
    val network: xml.Elem = XML.withSAXParser(XMLParserIgnoresDTD.parser).loadFile(networkFile)
    fromMATSimXML(network)
  }

  /**
    * loads a MATSim network from its xml definition
    * @param network xml tree matching the DTD description of network.xml for MATSim
    * @return either a local adjacency graph representation of a MATSim network, or, reports on failures from parsing
    */
  final def fromMATSimXML(network: xml.Elem): Either[String, LocalAdjacencyListFlowNetwork[Coordinate, EdgeBPR]] = {

    // add vertices to an empty graph
    val networkFileVertices = network \ "nodes" \ "node"
    val verticesBuilder: VerticesBuilder = networkFileVertices.foldLeft(VerticesBuilder())((verticesBuilder, node) => {
      val attrs: Map[String, String] = node.attributes.asAttrMap
      val name: VertexId             = VertexId(attrs("id").toString)
      val x: Double                  = attrs("x").toDouble
      val y: Double                  = attrs("y").toDouble
      verticesBuilder.copy(
        verticesBuilder.vertices.updated(name, Coordinate(x, y))
      )
    })

    // add edges to the graph with vertices in it, and return the result
    val networkFileEdges = network \ "links" \ "link"

    val edgesBuilder = networkFileEdges.foldLeft(EdgesBuilder())((edgesBuilder, link) => {

      val linkData: Map[String, String]            = link.attributes.asAttrMap
      val edgeId: EdgeId                               = EdgeId(linkData("id").toString)
      val src: VertexId                            = VertexId(linkData("from").toString)
      val dst: VertexId                            = VertexId(linkData("to").toString)
      val freespeedOption: Option[MetersPerSecond] = linkData.get("freespeed").map(fs => MetersPerSecond(toDoubleWithMinimum(fs, DefaultFreeSpeed)))
      val lanesOption: Option[Double]              = linkData.get("permlanes").map(toDoubleWithMinimum(_, DefaultLanes)) // default of 1 lane
      val lengthOption: Option[Meters]             = linkData.get("length").map(safeDistance(_))

      val capacityOption: Option[NaturalNumber] = {
        for {
          lanes  <- lanesOption
          length <- lengthOption
        } yield {
          (length * lanes) / VehicleSpacer
        }
      }.flatMap { cap =>
        NaturalNumber(cap.value.toInt).toOption
      }

      {
        for {
          freespeed <- freespeedOption
          capacity  <- capacityOption
          length    <- lengthOption
        } yield {
          EdgeBPR(length, freespeed, capacity)
        }
      } match {
        case None => edgesBuilder.copy(failedEdges = edgeId.value +: edgesBuilder.failedEdges)
        case Some(attr) =>
          val nextFwdAdjList =
            if (edgesBuilder.adjList.isDefinedAt(src))
              edgesBuilder.adjList.updated(src, edgesBuilder.adjList(src).updated(edgeId, dst))
            else
              edgesBuilder.adjList.updated(src, Map(edgeId -> dst))

          val nextRevAdjList =
            if (edgesBuilder.revAdjList.isDefinedAt(dst))
              edgesBuilder.revAdjList.updated(dst, edgesBuilder.revAdjList(dst).updated(edgeId, src))
            else
              edgesBuilder.revAdjList.updated(dst, Map(edgeId -> src))

          edgesBuilder.copy(
            edges = edgesBuilder.edges.updated(edgeId, RoadNetwork.EdgeTriplet(src, edgeId, dst, attr)),
            adjList = nextFwdAdjList,
            revAdjList = nextRevAdjList
          )
      }
    })

    if (verticesBuilder.failedVertices.nonEmpty || edgesBuilder.failedEdges.nonEmpty) {
      Left {
        s"""
           |vertices failed: ${verticesBuilder.failedVertices.length} - ids: ${verticesBuilder.failedVertices.mkString(", ")}\n
           |   edges failed: ${edgesBuilder.failedEdges.length} - ids: ${edgesBuilder.failedEdges.mkString(", ")}
           |""".stripMargin
      }
    } else {
      Right {
        LocalAdjacencyListFlowNetwork(
          edgesBuilder.edges,
          verticesBuilder.vertices,
          edgesBuilder.adjList,
          edgesBuilder.revAdjList
        )
      }
    }
  }

  def toDoubleWithMinimum(s: String, default: Double = Double.MinPositiveValue): Double = {
    Try {
      s.toDouble
    } match {
      case util.Failure(_) => default
      case util.Success(d) => math.max(default, d)
    }
  }

  def safeDistance(s: String, minNetworkDistance: Meters = Meters(10D)): Meters = {
    Try {
      s.toDouble
    } match {
      case util.Failure(_)     => minNetworkDistance
      case util.Success(value) => Meters(math.max(minNetworkDistance.value, value))
    }
  }
}

package edu.colorado.fitzgero.sotestbed.reports

import java.text.DecimalFormat

import cats.data.OptionT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Meters, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork.EdgeIdAndAttribute
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, PathSegment, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

object RouteReportOps {

  final case class DecisionTag(value: String) extends AnyVal {
    override def toString: String = value
  }

  object DecisionTag {
    def apply(time: SimTime, resultIndex: Int): DecisionTag = DecisionTag(s"$time#$resultIndex")
  }

  sealed trait PathType

  object PathType {

    final case object TrueShortestPath extends PathType {
      override def toString: String = "TSP"
    }

    final case object AltPath extends PathType {
      override def toString: String = "ALT"
    }

    final case object AltFiltered extends PathType {
      override def toString: String = "ALT_FILTERED"
    }
  }

  sealed trait PathRepresentation

  object PathRepresentation {

    // https://en.wikipedia.org/wiki/Decimal_degrees#Precision
    // 6 decimal places is unambigious for identifying individual humans
    val LatLonPrecisionFormat: DecimalFormat = new DecimalFormat("0.000000")

    final case class Linestring(path: List[Coordinate]) extends PathRepresentation {

      override def toString: String =
        path
          .map {
            case Coordinate(x, y) =>
              val xString: String = LatLonPrecisionFormat.format(x)
              val yString: String = LatLonPrecisionFormat.format(y)
              s"$xString $yString"
          }
          .mkString("\"LINESTRING (", ", ", ")\"")
    }

    final case class EdgeList(path: List[EdgeId]) extends PathRepresentation {

      override def toString: String =
        path.mkString("\"", ",", "\"")
    }

    final case class EdgesWithCosts[C](path: List[(EdgeId, C)], sep: Char = '|') extends PathRepresentation {

      override def toString: String =
        path.map { case (edgeId, cost) => s"$edgeId$sep$cost" }.mkString("\"", ",", "\"")
    }
  }

  def toCoords(roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR])(path: Path): List[Coordinate] = {
    val asCoordList: List[OptionT[IO, List[Coordinate]]] = for {
      (PathSegment(edgeId, _), edgeIdx) <- path.zipWithIndex
      if path.nonEmpty
    } yield for {
      srcVertex <- OptionT { roadNetwork.source(edgeId) }
      dstVertex <- OptionT { roadNetwork.destination(edgeId) }
      src       <- OptionT { roadNetwork.vertex(srcVertex) }
      dst       <- OptionT { roadNetwork.vertex(dstVertex) }
    } yield {
      if (edgeIdx == 0) {
        // use both coordinates
        List(src.attribute, dst.attribute)
      } else {
        // use only destination coordinate
        List(dst.attribute)
      }
    }
    asCoordList.map { _.value }.traverse { _.unsafeRunSync() } match {
      case None =>
        List.empty
      case Some(coordsWrapped) =>
        coordsWrapped.flatten
          .map { coordinate: Coordinate => coordinate }
    }
  }

  def pathDistance(roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR])(path: Path): Meters = {
    val asMeters: List[OptionT[IO, Meters]] = for {
      PathSegment(edgeId, _) <- path
      if path.nonEmpty
    } yield for {
      EdgeIdAndAttribute(_, attr) <- OptionT { roadNetwork.edge(edgeId) }
    } yield {
      attr.distance
    }
    asMeters.map { _.value }.traverse { _.unsafeRunSync() } match {
      case None                => Meters.Zero
      case Some(coordsWrapped) => coordsWrapped.foldLeft(Meters.Zero) { _ + _ }
    }
  }

  def pathEstTravelTime(roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR], costFunction: EdgeBPR => Cost)(
    path: Path
  ): Cost = {
    val asLinkCosts: List[OptionT[IO, Cost]] = for {
      PathSegment(edgeId, _) <- path
      if path.nonEmpty
    } yield for {
      EdgeIdAndAttribute(_, attr) <- OptionT { roadNetwork.edge(edgeId) }
    } yield {
      costFunction(attr)
    }
    asLinkCosts.map { _.value }.traverse { _.unsafeRunSync() } match {
      case None => Cost.Zero
      case Some(coordsWrapped) =>
        val result: Cost = coordsWrapped.foldLeft(Cost.Zero) { _ + _ }
        result
    }
  }

  /**
    * the time experienced is a set of observations at specific points in time.
    * the remaining trip is simply an estimate based on the current network state which is calculated here.
    *
    * allows us to witness how the agent's estimated overall trip duration changes over time.
    * @param latestRouteRequestData the latest information we have about the agent's trip
    * @return
    */
  def experiencedTravelTime(latestRouteRequestData: RouteRequestData): Cost = {
    // experienced route doesn't include current link
    val linkTravelTimes: List[Cost] = latestRouteRequestData.experiencedRoute
      .map { e => Cost(e.estimatedTimeAtEdge.getOrElse(SimTime.Zero).value) }
    val result: Cost = linkTravelTimes
      .foldLeft(Cost.Zero) { _ + _ }

    result
  }

  def dstCoord(roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR])(edgeId: EdgeId): Option[Coordinate] = {
    for {
      dstVertexId <- roadNetwork.destination(edgeId).unsafeRunSync()
      dstVertex   <- roadNetwork.vertex(dstVertexId).unsafeRunSync()
    } yield dstVertex.attribute
  }
}

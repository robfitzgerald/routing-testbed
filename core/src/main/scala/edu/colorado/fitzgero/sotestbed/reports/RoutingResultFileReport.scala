package edu.colorado.fitzgero.sotestbed.reports

import java.io.{File, PrintWriter}
import java.text.DecimalFormat

import cats.data.OptionT
import cats.effect.SyncIO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Meters, SimTime, TravelTimeSeconds}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork.EdgeIdAndAttribute
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, PathSegment, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.reports.RoutingResultFileReport.{DecisionTag, PathRepresentation, PathType, Row}

class RoutingResultFileReport(routingResultFile: File, costFunction: EdgeBPR => Cost) extends RoutingReports[SyncIO, Coordinate, EdgeBPR] {

  val printWriter: PrintWriter = new PrintWriter(routingResultFile)
  printWriter.write(RoutingResultFileReport.Header + "\n")

  override def updateReports(routingResults: List[RoutingAlgorithm.Result],
                             roadNetwork: RoadNetwork[SyncIO, Coordinate, EdgeBPR],
                             currentSimTime: SimTime): SyncIO[Unit] = SyncIO {

    val edgesToCoords: Path => List[Coordinate]   = RoutingResultFileReport.toCoords(roadNetwork)
    val edgeToCoord: EdgeId => Option[Coordinate] = RoutingResultFileReport.dstCoord(roadNetwork)
    val pathDistance: Path => Meters              = RoutingResultFileReport.pathDistance(roadNetwork)
    val pathEstTravelTime: Path => Cost           = RoutingResultFileReport.pathEstTravelTime(roadNetwork, costFunction)

    // gather all assets required to create routing report rows
    for {
      (routingResult, resultIndex) <- routingResults.zipWithIndex
      batchSize = routingResult.responses.length
      response <- routingResult.responses
      request                  = response.request
      observedRouteRequestData = routingResult.agentHistory.observedRouteRequestData.get(request.agent).map { _.orderedHistory }.getOrElse(List.empty)
      latestRouteRequestData <- routingResult.agentHistory.getMostRecentDataFor(request.agent)
      decisionNumber = observedRouteRequestData.length
      decisionTag    = DecisionTag(currentSimTime, resultIndex)
      Coordinate(lon, lat) <- edgeToCoord(request.origin)
      selectedPathIndex = response.pathIndex
      alts <- routingResult.kspResult.get(request)
      altsCoords = alts.map { path =>
        edgesToCoords(path)
      }
      altsFiltered <- routingResult.filteredKspResult.get(request)
      altsFilteredCoords = altsFiltered.map { path =>
        edgesToCoords(path)
      }
      if alts.nonEmpty && altsFiltered.nonEmpty
    } {
      // build a TSP row

      val trueShortestPath: PathRepresentation.Linestring = PathRepresentation.Linestring(altsCoords.head)
      val distanceExperienced                             = pathDistance(latestRouteRequestData.experiencedRoute.map { _.toPathSegment })
      val distanceRemaining                               = pathDistance(alts.head)
      val travelTimeExperienced                           = RoutingResultFileReport.experiencedTravelTime(latestRouteRequestData)
      val travelTimeRemaining                             = pathEstTravelTime(alts.head)
      val tspRow: Row = Row(
        agentId = request.agent,
        time = currentSimTime,
        decision = decisionNumber,
        decisionTag = decisionTag,
        batchSize = batchSize,
        rowType = PathType.TrueShortestPath,
        alt = 0,
        selected = selectedPathIndex == 0,
        distanceExperienced = distanceExperienced,
        distanceRemaining = distanceRemaining,
        distanceOverall = distanceExperienced + distanceRemaining,
        timeExperienced = travelTimeExperienced,
        timeEstRemaining = travelTimeRemaining,
        timeEstOverall = travelTimeExperienced + travelTimeRemaining,
        currentLink = request.origin,
        lat = lat,
        lon = lon,
        path = trueShortestPath
      )
      // build all alts rows
      val altPaths: List[Row] = alts.zip(altsCoords).zipWithIndex.tail.map {
        case ((path, pathCoords), idx) =>
          val distanceExperienced   = pathDistance(latestRouteRequestData.experiencedRoute.map { _.toPathSegment })
          val distanceRemaining     = pathDistance(path)
          val travelTimeExperienced = RoutingResultFileReport.experiencedTravelTime(latestRouteRequestData)
          val travelTimeRemaining   = pathEstTravelTime(path)
          Row(
            agentId = request.agent,
            time = currentSimTime,
            decision = decisionNumber,
            decisionTag = decisionTag,
            batchSize = batchSize,
            rowType = PathType.AltPath,
            alt = idx,
            selected = selectedPathIndex == idx,
            distanceExperienced = distanceExperienced,
            distanceRemaining = distanceRemaining,
            distanceOverall = distanceExperienced + distanceRemaining,
            timeExperienced = travelTimeExperienced,
            timeEstRemaining = travelTimeRemaining,
            timeEstOverall = travelTimeExperienced + travelTimeRemaining,
            currentLink = request.origin,
            lat = lat,
            lon = lon,
            path = PathRepresentation.Linestring(pathCoords)
          )
      }
      // build all filtered alt rows
      val filteredAltPaths: List[Row] = altsFiltered.zip(altsFilteredCoords).zipWithIndex.map {
        case ((path, pathCoords), idx) =>
          val distanceExperienced   = pathDistance(latestRouteRequestData.experiencedRoute.map { _.toPathSegment })
          val distanceRemaining     = pathDistance(path)
          val travelTimeExperienced = RoutingResultFileReport.experiencedTravelTime(latestRouteRequestData)
          val travelTimeRemaining   = pathEstTravelTime(path)
          Row(
            agentId = request.agent,
            time = currentSimTime,
            decision = decisionNumber,
            decisionTag = decisionTag,
            batchSize = batchSize,
            rowType = PathType.AltFiltered,
            alt = idx,
            selected = selectedPathIndex == idx,
            distanceExperienced = distanceExperienced,
            distanceRemaining = distanceRemaining,
            distanceOverall = distanceExperienced + distanceRemaining,
            timeExperienced = travelTimeExperienced,
            timeEstRemaining = travelTimeRemaining,
            timeEstOverall = travelTimeExperienced + travelTimeRemaining,
            currentLink = request.origin,
            lat = lat,
            lon = lon,
            path = PathRepresentation.Linestring(pathCoords)
          )
      }

      val allRows: List[Row] = tspRow +: altPaths ::: filteredAltPaths

//      val avgAltsPerAgent: Double =
//        if (routingResult.kspResult.isEmpty) 0
//        else routingResult.kspResult.map { _._2.length }.sum.toDouble / routingResult.kspResult.size

      // print all rows for this agent
      for {
        row <- allRows
      } {
        printWriter.write(row.toString)
      }
    }
  }

  def close(): Unit = {
    printWriter.close()
  }
}

object RoutingResultFileReport {

  // header for the route data output file
  val Header: String =
    "agentId,time,decision,decisionTag,batchSize,rowType,alt,selected,distExperienced,distRemaining,distOverall,timeExperienced,timeEstRemaining,timeOverall,currentLink,lat,lon,path"

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

  final case class Row(
    agentId: String,
    time: SimTime,
    decision: Int,
    decisionTag: DecisionTag,
    batchSize: Int,
    rowType: PathType,
    alt: Int,
    selected: Boolean,
    distanceExperienced: Meters,
    distanceRemaining: Meters,
    distanceOverall: Meters,
    timeExperienced: Cost,
    timeEstRemaining: Cost,
    timeEstOverall: Cost,
    currentLink: EdgeId,
    lat: Double,
    lon: Double,
    path: PathRepresentation
  ) {
    override def toString: String =
      s"$agentId,$time,$decision,$decisionTag,$batchSize,$rowType,$alt,$selected,$distanceExperienced,$distanceRemaining,$distanceOverall,$timeExperienced,$timeEstRemaining,$timeEstOverall,$currentLink,$lat,$lon,$path\n"
  }

  def toCoords(roadNetwork: RoadNetwork[SyncIO, Coordinate, EdgeBPR])(path: Path): List[Coordinate] = {
    val asCoordList: List[OptionT[SyncIO, List[Coordinate]]] = for {
      (PathSegment(edgeId, _), edgeIdx) <- path.zipWithIndex
      if path.nonEmpty
    } yield
      for {
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
      case None                => List.empty
      case Some(coordsWrapped) => coordsWrapped.flatten
    }
  }

  def pathDistance(roadNetwork: RoadNetwork[SyncIO, Coordinate, EdgeBPR])(path: Path): Meters = {
    val asMeters: List[OptionT[SyncIO, Meters]] = for {
      PathSegment(edgeId, _) <- path
      if path.nonEmpty
    } yield
      for {
        EdgeIdAndAttribute(_, attr) <- OptionT { roadNetwork.edge(edgeId) }
      } yield {
        attr.distance
      }
    asMeters.map { _.value }.traverse { _.unsafeRunSync() } match {
      case None                => Meters.Zero
      case Some(coordsWrapped) => coordsWrapped.foldLeft(Meters.Zero) { _ + _ }
    }
  }

  def pathEstTravelTime(roadNetwork: RoadNetwork[SyncIO, Coordinate, EdgeBPR], costFunction: EdgeBPR => Cost)(path: Path): Cost = {
    val asLinkCosts: List[OptionT[SyncIO, Cost]] = for {
      PathSegment(edgeId, _) <- path
      if path.nonEmpty
    } yield
      for {
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
      .map { e =>
        Cost(e.estimatedTimeAtEdge.getOrElse(SimTime.Zero).value)
      }
    val result: Cost = linkTravelTimes
      .foldLeft(Cost.Zero) { _ + _ }

    result
  }

  def dstCoord(roadNetwork: RoadNetwork[SyncIO, Coordinate, EdgeBPR])(edgeId: EdgeId): Option[Coordinate] = {
    for {
      dstVertexId <- roadNetwork.destination(edgeId).unsafeRunSync()
      dstVertex   <- roadNetwork.vertex(dstVertexId).unsafeRunSync()
    } yield dstVertex.attribute
  }
}

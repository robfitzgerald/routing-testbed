package edu.colorado.fitzgero.sotestbed.reports

import java.io.{File, PrintWriter}
import java.text.DecimalFormat

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Meters, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.reports.CompletePathAlternativesRoutingReport.Row
import edu.colorado.fitzgero.sotestbed.reports.RouteReportOps.{DecisionTag, PathRepresentation, PathType}

class CompletePathAlternativesRoutingReport(routingResultFile: File, costFunction: EdgeBPR => Cost)
    extends RoutingReports[IO, Coordinate, EdgeBPR] {

  val printWriter: PrintWriter = new PrintWriter(routingResultFile)
  printWriter.write(CompletePathAlternativesRoutingReport.Header + "\n")

  override def updateReports(
    routingResults: List[(String, RoutingAlgorithm.Result)],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    currentSimTime: SimTime
  ): IO[Unit] = IO {

    // functions to interpret path space data
    val edgesToCoords: Path => List[Coordinate]   = RouteReportOps.toCoords(roadNetwork)
    val edgeToCoord: EdgeId => Option[Coordinate] = RouteReportOps.dstCoord(roadNetwork)
    val pathDistance: Path => Meters              = RouteReportOps.pathDistance(roadNetwork)
    val pathEstTravelTime: Path => Cost           = RouteReportOps.pathEstTravelTime(roadNetwork, costFunction)

    // gather all assets required to create routing report rows
    for {
      ((batchId, routingResult), resultIndex) <- routingResults.zipWithIndex
      batchSize = routingResult.kspResult.size
      response <- routingResult.responses
      request = response.request
      observedRouteRequestData = routingResult.agentHistory.observedRouteRequestData
        .get(request.agent)
        .map { _.orderedHistory }
        .getOrElse(List.empty)
      latestRouteRequestData <- routingResult.agentHistory.getMostRecentDataFor(request.agent)
      decisionNumber = observedRouteRequestData.length
      decisionTag    = DecisionTag(currentSimTime, resultIndex)
      Coordinate(lon, lat) <- edgeToCoord(request.location)
      selectedPathIndex = response.pathIndex
      alts <- routingResult.kspResult.get(request)
      altsCoords = alts.map { path => edgesToCoords(path) }
      altsFiltered <- routingResult.filteredKspResult.get(request)
      altsFilteredCoords = altsFiltered.map { path => edgesToCoords(path) }
      samples            = routingResult.samples
      searchSpaceSize = if (routingResult.kspResult.nonEmpty)
        routingResult.kspResult.values.map { _.length.toDouble }.product
      else 0
      if alts.nonEmpty && altsFiltered.nonEmpty
    } {
      // build a TSP row

      val trueShortestPath: PathRepresentation.Linestring = PathRepresentation.Linestring(altsCoords.head)
      val distanceExperienced                             = pathDistance(latestRouteRequestData.experiencedRoute.map { _.toPathSegment })
      val distanceRemaining                               = pathDistance(alts.head)
      val travelTimeExperienced                           = RouteReportOps.experiencedTravelTime(latestRouteRequestData)
      val travelTimeRemaining                             = pathEstTravelTime(alts.head)
      val tspRow: Row = Row(
        agentId = request.agent,
        batchId = batchId,
        time = currentSimTime,
        decision = decisionNumber,
        decisionTag = decisionTag,
        batchSize = batchSize,
        rowType = PathType.TrueShortestPath,
        alt = 0,
        selected = selectedPathIndex == 0,
        samples = samples,
        searchSpace = searchSpaceSize,
        distanceExperienced = distanceExperienced,
        distanceRemaining = distanceRemaining,
        distanceOverall = distanceExperienced + distanceRemaining,
        timeExperienced = travelTimeExperienced,
        timeEstRemaining = travelTimeRemaining,
        timeEstOverall = travelTimeExperienced + travelTimeRemaining,
        currentLink = request.location,
        lat = lat,
        lon = lon,
        path = trueShortestPath
      )
      // build all alts rows
      val altPaths: List[Row] = alts.zip(altsCoords).zipWithIndex.tail.map {
        case ((path, pathCoords), idx) =>
          val distanceExperienced   = pathDistance(latestRouteRequestData.experiencedRoute.map { _.toPathSegment })
          val distanceRemaining     = pathDistance(path)
          val travelTimeExperienced = RouteReportOps.experiencedTravelTime(latestRouteRequestData)
          val travelTimeRemaining   = pathEstTravelTime(path)
          Row(
            agentId = request.agent,
            batchId = batchId,
            time = currentSimTime,
            decision = decisionNumber,
            decisionTag = decisionTag,
            batchSize = batchSize,
            rowType = PathType.AltPath,
            alt = idx,
            selected = selectedPathIndex == idx,
            samples = samples,
            searchSpace = searchSpaceSize,
            distanceExperienced = distanceExperienced,
            distanceRemaining = distanceRemaining,
            distanceOverall = distanceExperienced + distanceRemaining,
            timeExperienced = travelTimeExperienced,
            timeEstRemaining = travelTimeRemaining,
            timeEstOverall = travelTimeExperienced + travelTimeRemaining,
            currentLink = request.location,
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
          val travelTimeExperienced = RouteReportOps.experiencedTravelTime(latestRouteRequestData)
          val travelTimeRemaining   = pathEstTravelTime(path)
          Row(
            agentId = request.agent,
            batchId = batchId,
            time = currentSimTime,
            decision = decisionNumber,
            decisionTag = decisionTag,
            batchSize = batchSize,
            rowType = PathType.AltFiltered,
            alt = idx,
            selected = selectedPathIndex == idx,
            samples = samples,
            searchSpace = searchSpaceSize,
            distanceExperienced = distanceExperienced,
            distanceRemaining = distanceRemaining,
            distanceOverall = distanceExperienced + distanceRemaining,
            timeExperienced = travelTimeExperienced,
            timeEstRemaining = travelTimeRemaining,
            timeEstOverall = travelTimeExperienced + travelTimeRemaining,
            currentLink = request.location,
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

object CompletePathAlternativesRoutingReport {

  // header for the route data output file
  val Header: String =
    "agentId,batchId,time,decision,decisionTag,batchSize,rowType,alt,selected,samples,searchSpace,spaceExploredPercent,distExperienced,distRemaining,distOverall,distTraveledPercent,timeExperienced,timeEstRemaining,timeEstOverall,currentLink,lat,lon,path"

  val LatLonPrecisionFormat: DecimalFormat = new DecimalFormat("0.000000")

  final case class Row(
    agentId: String,
    batchId: String,
    time: SimTime,
    decision: Int,
    decisionTag: DecisionTag,
    batchSize: Int,
    rowType: PathType,
    alt: Int,
    selected: Boolean,
    samples: Int,
    searchSpace: Double,
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

    override def toString: String = {
      val spaceExplored: String    = f"${(samples.toDouble / searchSpace) * 100.0}%.2f%%"
      val distanceTraveled: String = f"${(distanceExperienced.value / distanceOverall.value) * 100.0}%.2f%%"
      val latString: String        = LatLonPrecisionFormat.format(lat)
      val lonString: String        = LatLonPrecisionFormat.format(lon)
      s"$agentId,$batchId,$time,$decision,$decisionTag,$batchSize,$rowType,$alt,$selected,$samples,$searchSpace,$spaceExplored,$distanceExperienced,$distanceRemaining,$distanceOverall,$distanceTraveled,$timeExperienced,$timeEstRemaining,$timeEstOverall,$currentLink,$latString,$lonString,$path\n"
    }
  }
}

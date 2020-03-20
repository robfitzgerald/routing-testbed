package edu.colorado.fitzgero.sotestbed.reports

import java.io.{File, PrintWriter}

import cats.effect.SyncIO

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Meters, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.reports.RouteReportOps.{DecisionTag, PathType}

class AggregateDataRoutingReport(routingResultFile: File, costFunction: EdgeBPR => Cost) extends RoutingReports[SyncIO, Coordinate, EdgeBPR] {

  val printWriter: PrintWriter = new PrintWriter(routingResultFile)
  printWriter.write(AggregateDataRoutingReport.Header + "\n")

  override def updateReports(routingResults: List[RoutingAlgorithm.Result],
                             roadNetwork: RoadNetwork[SyncIO, Coordinate, EdgeBPR],
                             currentSimTime: SimTime): SyncIO[Unit] = SyncIO {

    // functions to interpret path space data
    val edgesToCoords: Path => List[Coordinate]   = RouteReportOps.toCoords(roadNetwork)
    val edgeToCoord: EdgeId => Option[Coordinate] = RouteReportOps.dstCoord(roadNetwork)
    val pathDistance: Path => Meters              = RouteReportOps.pathDistance(roadNetwork)
    val pathEstTravelTime: Path => Cost           = RouteReportOps.pathEstTravelTime(roadNetwork, costFunction)

    println("here")

    // gather all assets required to create routing report rows
    for {
      (routingResult, resultIndex) <- routingResults.zipWithIndex
      batchSize = routingResult.kspResult.size
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
      samples         = routingResult.samples
      searchSpaceSize = if (routingResult.kspResult.nonEmpty) routingResult.kspResult.values.map { _.length.toDouble }.product else 0
      if alts.nonEmpty
    } {
      // build a TSP row

      val altsWithCoordsAndIndices  = alts.zip(altsCoords).zipWithIndex
      val selected                  = altsWithCoordsAndIndices(selectedPathIndex)
      val ((path, pathCoords), idx) = selected

      val distanceExperienced = pathDistance(latestRouteRequestData.experiencedRoute.map {
        _.toPathSegment
      })
      val distanceRemaining     = pathDistance(path)
      val travelTimeExperienced = RouteReportOps.experiencedTravelTime(latestRouteRequestData)
      val travelTimeRemaining   = pathEstTravelTime(path)

      val row = AggregateDataRoutingReport.Row(
        agentId = request.agent,
        time = currentSimTime,
        decision = decisionNumber,
        decisionTag = decisionTag,
        batchSize = batchSize,
        rowType = PathType.AltPath,
        alt = idx,
        samples = samples,
        searchSpace = searchSpaceSize,
        distanceExperienced = distanceExperienced,
        distanceRemaining = distanceRemaining,
        distanceOverall = distanceExperienced + distanceRemaining,
        timeExperienced = travelTimeExperienced,
        timeEstRemaining = travelTimeRemaining,
        timeEstOverall = travelTimeExperienced + travelTimeRemaining,
        currentLink = request.origin,
        lat = lat,
        lon = lon,
      )

      printWriter.write(row.toString)
    }
  }

  def close(): Unit = {
    printWriter.close()
  }
}

object AggregateDataRoutingReport {

  // header for the route data output file
  val Header: String =
    "agentId,time,decision,decisionTag,batchSize,rowType,alt,samples,searchSpace,spaceExploredPercent,distExperienced,distRemaining,distOverall,distTraveledPercent,timeExperienced,timeEstRemaining,timeOverall,currentLink,lat,lon"

  final case class Row(
    agentId: String,
    time: SimTime,
    decision: Int,
    decisionTag: DecisionTag,
    batchSize: Int,
    rowType: PathType,
    alt: Int,
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
  ) {
    override def toString: String = {
      val spaceExplored: String    = f"${(samples.toDouble / searchSpace) * 100.0}%.2f%%"
      val distanceTraveled: String = f"${(distanceExperienced.value / distanceOverall.value) * 100.0}%.2f%%"
      s"$agentId,$time,$decision,$decisionTag,$batchSize,$rowType,$alt,$samples,$searchSpace,$spaceExplored,$distanceExperienced,$distanceRemaining,$distanceOverall,$distanceTraveled,$timeExperienced,$timeEstRemaining,$timeEstOverall,$currentLink,$lat,$lon\n"
    }
  }
}

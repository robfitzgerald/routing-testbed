package edu.colorado.fitzgero.sotestbed.reports

import java.io.{File, PrintWriter}
import java.nio.file.Path

import scala.collection.JavaConverters._

import cats.effect.IO

import com.uber.h3core.H3Core
import com.uber.h3core.util.GeoCoord
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, MetersPerSecond, SimTime, TravelTimeSeconds}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetwork}
import org.locationtech.proj4j.{BasicCoordinateTransform, CRSFactory, CoordinateTransform, ProjCoordinate}

case class AvgSpeedHeatmap(
  h3Core: H3Core,
  h3Mapping: AvgSpeedHeatmap.H3Mapping,
  heatmapPrintWriter: PrintWriter,
  costFunction: EdgeBPR => Cost
) {

  /**
    * append a set of rows to the heatmap file corresponding with the current network state and a timestamp
    * @param currentNetworkState the state of the network at the current time
    * @param currentTime the simulation time
    */
  def appendHeatmapWithTimeWindowedData(
    currentNetworkState: RoadNetwork[IO, Coordinate, EdgeBPR],
    currentTime: SimTime
  ): Unit = {
    val allObservedSpeedsPerHex: Map[Long, List[Double]] =
      currentNetworkState.edgeTriplets
        .unsafeRunSync()
        .foldLeft(Map.empty[Long, List[Double]]) { (acc, link) =>
          if (link.attr.flow == Flow.Zero) {
            // ignore unloaded links
            acc
          } else
            h3Mapping.get(link.edgeId) match {
              case None =>
                // handle error?
                acc
              case Some(hex) =>
                val travelTime: TravelTimeSeconds    = TravelTimeSeconds(costFunction(link.attr).value)
                val metersPerSecond: MetersPerSecond = MetersPerSecond(link.attr.distance, travelTime)
                val mph: Double                      = metersPerSecond.asMph
                val updatedSpeeds: List[Double]      = mph +: acc.getOrElse(hex, List.empty)
                acc.updated(hex, updatedSpeeds)
            }
        }

    for {
      (hex, speeds) <- allObservedSpeedsPerHex
    } {
      val row = f"$currentTime,$hex,${speeds.sum / speeds.size}%.2f"
      heatmapPrintWriter.write(row + "\n")
    }
  }

  /**
    * cleans up the open connections
    */
  def close(): Unit = {
    heatmapPrintWriter.close()
  }
}

object AvgSpeedHeatmap {

  type H3Mapping = Map[EdgeId, Long]

  val HeatmapFileHeader: String = "time,hex,avgSpeed"

  /**
    *
    * @param initialGraph the graph representation of this run
    * @param loggingDirectory the directory where logging logs to directories
    * @param h3Resolution h3 resolution which sets the hex size; see https://github.com/uber/h3/blob/master/docs/core-library/restable.md
    * @param networkCRS as of right now, we use web mercator (EPSG:3857) exclusively in our MATSim runs - 20200706
    * @param avgSpeedHeatmapFileName name of output heatmap file
    * @param polygonLookupFileName name of output polygons file
    * @return
    */
  def apply(
    initialGraph: RoadNetwork[IO, Coordinate, EdgeBPR],
    loggingDirectory: Path,
    costFunction: EdgeBPR => Cost,
    h3Resolution: Int = 7, // 5 square kilometers per hex
    networkCRS: String = "EPSG:3857", // web mercator
    avgSpeedHeatmapFileName: String = "avgSpeedHeatmap.csv",
    polygonLookupFileName: String = "h3Polygons.csv",
  ): AvgSpeedHeatmap = {

    val h3Core: H3Core                       = H3Core.newInstance()
    val h3Mapping: AvgSpeedHeatmap.H3Mapping = AvgSpeedHeatmap.createMapping(initialGraph, h3Resolution, networkCRS, h3Core)

    val heatmapPrintWriter: PrintWriter = createHeatmapFile(
      loggingDirectory = loggingDirectory,
      h3Mapping = h3Mapping,
      h3Core = h3Core,
      avgSpeedHeatmapFileName = avgSpeedHeatmapFileName,
      polygonLookupFileName = polygonLookupFileName
    )

    AvgSpeedHeatmap(
      h3Core = h3Core,
      h3Mapping = h3Mapping,
      heatmapPrintWriter = heatmapPrintWriter,
      costFunction = costFunction
    )
  }

  /**
    * creates a mapping from [[EdgeId]]s to h3 hexes
    * @param graph the graph, mined for edge midpoint coordinates
    * @param h3Resolution h3 resolution which sets the hex size
    * @param networkCRS coordinate reference system of the network
    * @param h3Core an instance of the H3 library
    * @return a mapping between EdgeIds and h3 hexes
    */
  def createMapping(graph: RoadNetwork[IO, Coordinate, EdgeBPR], h3Resolution: Int, networkCRS: String, h3Core: H3Core): AvgSpeedHeatmap.H3Mapping = {

    val crsFactory: CRSFactory  = new CRSFactory()
    val ct: CoordinateTransform = new BasicCoordinateTransform(crsFactory.createFromName(networkCRS), crsFactory.createFromName("EPSG:4326"))

    val linkWithHex: Iterable[(EdgeId, Long)] = for {
      link <- graph.edgeTriplets.unsafeRunSync()
      src  <- graph.vertex(link.src).unsafeRunSync()
      dst  <- graph.vertex(link.dst).unsafeRunSync()
    } yield {
      val midX: Double   = (src.attribute.x + dst.attribute.x) / 2.0
      val midY: Double   = (src.attribute.y + dst.attribute.y) / 2.0
      val midpoint       = new ProjCoordinate(midX, midY)
      var midpointLatLon = new ProjCoordinate()
      ct.transform(midpoint, midpointLatLon)
      (link.edgeId, h3Core.geoToH3(midpointLatLon.x, midpointLatLon.y, h3Resolution))
    }

    linkWithHex.toMap
  }

  /**
    * creates the heatmap file (writes header) and creates h3 polygon lookup table in logging directory
    *
    * @param loggingDirectory the place where output logging files are placed
    * @param h3Mapping a mapping from edgeid to h3 hex
    * @param h3Core an instance of the h3 library
    * @param avgSpeedHeatmapFileName filename for the heatmap file
    * @param polygonLookupFileName filename for the polygon lookup table
    * @return the file handler for the heatmap file
    */
  def createHeatmapFile(
    loggingDirectory: Path,
    h3Mapping: H3Mapping,
    h3Core: H3Core,
    avgSpeedHeatmapFileName: String,
    polygonLookupFileName: String
  ): PrintWriter = {

    // set up the heatmap file output
    val avgSpeedHeatmapFile: File      = loggingDirectory.resolve(avgSpeedHeatmapFileName).toFile
    val heatmapFileWriter: PrintWriter = new PrintWriter(avgSpeedHeatmapFile)
    heatmapFileWriter.write(AvgSpeedHeatmap.HeatmapFileHeader + "\n")
    heatmapFileWriter.flush()

    // create a polygon lookup table
    val polygonLookupFile: File        = loggingDirectory.resolve(polygonLookupFileName).toFile
    val polygonFileWriter: PrintWriter = new PrintWriter(polygonLookupFile)
    polygonFileWriter.write("hex,WKT\n")

    for {
      hex: Long <- h3Mapping.values.toSet.toList
    } yield {
      val boundary: List[GeoCoord] = h3Core.h3ToGeoBoundary(hex).asScala.toList
      val wkt: String = (boundary.last +: boundary)
        .map { c =>
          f"${c.lat} ${c.lng}"
        }
        .mkString("\"POLYGON((", ", ", "))\"")
      polygonFileWriter.write(f"$hex,$wkt\n")
    }

    polygonFileWriter.close()

    // return the heatmap PrintWriter
    heatmapFileWriter
  }
}

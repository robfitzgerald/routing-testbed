package edu.colorado.fitzgero.sotestbed.reports

import java.io.{File, PrintWriter}
import java.nio.file.Path

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

class ReplanningCoordinateReporter(pw: PrintWriter) extends RoutingReports[IO, Coordinate, EdgeBPR] {

  def updateReports(
    routingResult: List[(String, RoutingAlgorithm.Result)],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    currentTime: SimTime
  ): IO[Unit] = {
    val rows = for {
      (batchId, result) <- routingResult
      response          <- result.responses
      if batchId != "ue"
    } yield {
      val agent  = response.request.agent
      val edgeId = response.request.location
      for {
        srcIdOpt <- roadNetwork.source(edgeId)
        srcId    <- IO.fromOption(srcIdOpt)(new Error(s"edge $edgeId missing src vertex"))
        srcOpt   <- roadNetwork.vertex(srcId)
        src      <- IO.fromOption(srcOpt)(new Error(s"edge $edgeId missing src vertex"))
      } yield {
        val (x, y) = (src.attribute.x, src.attribute.y)
        val wkt    = f""""POINT($x $y)""""
        s"$agent,$batchId,$currentTime,$x,$y,$wkt" + "\n"
      }
    }

    val result = rows.sequence.map { rows => rows.foreach(pw.write) }

    pw.flush()

    result
  }

  def close(): Unit = pw.close()
}

object ReplanningCoordinateReporter {

  val Filename: String = "replanningCoordinates.csv"

  val Header: String = "agentId,batchId,time,x,y,wkt"

  def apply(outputDirectory: Path): ReplanningCoordinateReporter = {
    val outFile                  = outputDirectory.resolve(Filename).toFile
    val printWriter: PrintWriter = new PrintWriter(outFile)
    printWriter.write(Header + "\n")
    new ReplanningCoordinateReporter(printWriter)
  }
}

package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.File

import kantan.csv._
import kantan.csv.ops._
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, PrecisionModel}

object PopulationSamplingOps {

  /**
    * takes a csv file with lat/lon values and reads it in as a boundary for the road network for sampling populations
    *
    * assumed to be in WGS84 (SRID 4326) CRS
    *
    * @param geometryFile the CSV file containing the geometry
    * @param geometrySRID the SRID of the polygon geometry
    * @return
    */
  def readBoundingGeometryFile(geometryFile: File, geometrySRID: Int = 4326): Either[Exception, Geometry] = {

    val geometryFactory: GeometryFactory = new GeometryFactory(new PrecisionModel(), geometrySRID)
    implicit val headerDecoder: HeaderDecoder[Coordinate] = HeaderDecoder.decoder("lat", "lon") { (lat: Double, lon: Double) =>
      new Coordinate(lat, lon)
    }
    case class Acc(points: List[Coordinate] = List.empty, errors: List[String] = List.empty)
    val result: Acc = geometryFile
      .asCsvReader[Coordinate](rfc.withHeader)
      .foldLeft(Acc()) { (acc, row) =>
        row match {
          case Left(e) =>
            acc.copy(errors = f"${e.getCause} ${e.getMessage}" +: acc.errors)
          case Right(point) =>
            acc.copy(points = point +: acc.points)
        }
      }
    if (result.errors.nonEmpty) {
      Left(new Exception(result.errors.mkString("failures while parsing polygon file:\n", "\n", "")))
    } else {
      val geometry = geometryFactory.createPolygon(result.points.toArray)
      Right(geometry)
    }
  }
}

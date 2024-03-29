package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.File
import java.nio.file.Files
import scala.io.Source

import kantan.csv._
import kantan.csv.ops._
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, PrecisionModel}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population._
import org.locationtech.jts.io.WKTReader
import org.locationtech.jts.io.geojson.GeoJsonReader
import scala.util.Try
import cats.effect.IO
import java.time.LocalTime
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimRunConfig
import java.nio.file.Path

object PopulationSamplingOps {

  /**
    * checks if a population file exists matching the provided configuration and (optional) trial # (for repeater
    * app runs). if not found, run the population generation algorithm in this run config.
    *
    * @param matsimRunConfig
    * @param trial
    * @return
    */
  def buildPopulationIfMissing(conf: MATSimRunConfig): Either[Error, Path] = {
    val outDirPath = conf.trialSharedDirectory
    val filepath   = conf.populationFilepath

    if (filepath.toFile.isFile) Right(filepath)
    else {
      for {
        _ <- Try { Files.createDirectories(outDirPath) }.toEither.left.map { t =>
          new Error(f"unable to create directory $outDirPath")
        }
        _ <- PopSamplingAlgorithm.generatePopulation(conf.population, filepath, conf.trial)
      } yield filepath
    }
  }

  /**
    * reads in a GeoJSON file
    * @deprecated
    */
  def readBoundingGeometryGeoJson(geometryFile: File, srid: Int = 4326): Either[Exception, Geometry] = {

    val result = for {
      reader <- Try { new GeoJsonReader(new GeometryFactory(new PrecisionModel(), srid)) }
      source <- Try { Source.fromFile(geometryFile) }
      string <- Try { source.getLines.mkString }
      _ = source.close()
      geometry <- Try { reader.read(string) }
    } yield geometry

    result.toEither.left.map { t => new Exception(f"reading GeoJson failed", t) }
  }

  /**
    * takes a csv file with lat/lon values and reads it in as a boundary for the road network for sampling populations
    *
    * assumed to be in WGS84 (SRID 4326) CRS
    *
    * @param geometryFile the CSV file containing the geometry
    * @param geometrySRID the SRID of the polygon geometry
    * @return
    */
  def readBoundingGeometryXYCsv(geometryFile: File, geometrySRID: Int = 4326): Either[Exception, Geometry] = {

    val geometryFactory: GeometryFactory = new GeometryFactory(new PrecisionModel(), geometrySRID)
    implicit val headerDecoder: HeaderDecoder[Coordinate] = HeaderDecoder.decoder("lat", "lon") {
      (lat: Double, lon: Double) => new Coordinate(lat, lon)
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

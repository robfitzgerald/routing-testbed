package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population

import java.io.File
import org.locationtech.jts.io.WKTReader
import kantan.csv._
import kantan.csv.ops._
import cats.implicits._
import scala.util.Try
import org.locationtech.jts.geom._
import org.geotools.referencing.CRS
import org.geotools.geometry.jts.JTS
import org.opengis.referencing.operation.MathTransform
import com.typesafe.scalalogging.LazyLogging

object PopSamplingFileOps extends LazyLogging {

  /**
    * reads a CSV file with a WKT column and an id column. transforms it to a target
    * srid
    *
    * @param file
    * @param srid
    * @param idCol
    * @param geomCol
    * @return
    */
  def readWktCsv(
    file: File,
    srid: Int,
    idCol: String,
    geomCol: String,
    targetSrid: Int
  ): Either[Error, List[(String, Geometry)]] = {
    val wrappedResult = Try {
      val gf = new GeometryFactory(new PrecisionModel(), srid)
      val transform: Option[MathTransform] =
        if (srid == targetSrid) None
        else Some(CRS.findMathTransform(CRS.decode(f"EPSG:$srid"), CRS.decode(f"EPSG:$targetSrid"), true))
      val reader                              = new WKTReader(gf)
      implicit val gcd: CellDecoder[Geometry] = CellDecoder.from(readWktRow(reader, transform))
      case class Row(id: String, geometry: Geometry)
      implicit val hd: HeaderDecoder[Row] = HeaderDecoder.decoder(idCol, geomCol)(Row.apply)
      // val conf                                = rfc.withHeader(idCol, geomCol)
      val result = ReadResult.sequence(file.readCsv[List, Row](rfc.withHeader))
      result.map(_.map(row => (row.id, row.geometry)))
    }

    wrappedResult.toEither.flatten.left.map { t => new Error(f"failure reading WKT CSV file", t) }
  }

  def readWktRow(reader: WKTReader, t: Option[MathTransform])(geomStr: String): Either[DecodeError, Geometry] =
    Try {
      val geom = reader.read(geomStr)
      t match {
        case None            => geom
        case Some(transform) => JTS.transform(geom, transform)
      }
    }.toEither.left.map { t => DecodeError.TypeError(s"failed decoding geomtry WKT: '$geomStr'") }

  /**
    * reads a CSV file with OD demand by zone
    *
    * @param file
    * @param srcCol
    * @param dstCol
    * @param startTimeCol
    * @param endTimeCol
    * @param countCol
    * @param sep
    * @return
    */
  def readDemandTableCsv(
    file: File,
    srcCol: String,
    dstCol: String,
    binNameCol: String,
    startTimeCol: String,
    endTimeCol: String,
    countCol: String,
    sep: Char
  ): Either[Error, List[DemandTableRow]] = {
    implicit val hd: HeaderDecoder[DemandTableRow] =
      DemandTableRow.headerDecoder(srcCol, dstCol, binNameCol, startTimeCol, endTimeCol, countCol)
    val conf = rfc.withCellSeparator(sep).withHeader
    val readResult = file
      .asCsvReader[DemandTableRow](conf)
      .toList
      .sequence

    // no uniqueness guarantees; more than one row could share the same source taz + destination
    // taz + bin name combination. let's aggregate them here.
    val aggregated = readResult.map {
      _.foldLeft(Map.empty[String, DemandTableRow]) { (acc, row) =>
        val key = f"${row.src}-${row.dst}-${row.bin}"
        acc.get(key) match {
          case None           => acc.updated(key, row)
          case Some(existing) => acc.updated(key, row.copy(cnt = row.cnt + existing.cnt))
        }
      }.values.toList
    }

    aggregated.left.map { t => new Error(s"failure reading demand table file", t) }
  }
}

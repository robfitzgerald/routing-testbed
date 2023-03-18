package edu.colorado.fitzgero.sotestbed.algorithm.grid

import scala.collection.JavaConverters._
import scala.util.Try

import org.locationtech.jts.geom.{Coordinate, GeometryFactory, Polygon, PrecisionModel}
import org.locationtech.jts.index.strtree.STRtree
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import cats.implicits._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import java.io.File
import kantan.csv._
import kantan.csv.ops._
import org.locationtech.jts.io.WKTReader
import com.typesafe.scalalogging.LazyLogging

/**
  * revised coordinate grid which uses JTS polygons + spatial tree search
  * which also can be printed to a .csv with columns grid_id,wkt (in WGS84)
  * so we can overlay the grid on top of the road network for visualizations
  */
final class CoordinateGrid2(
  val gridCells: Map[String, Polygon],
  geometryFactory: GeometryFactory,
  private val lookup: STRtree,
  val edgeLookup: Map[String, List[EdgeId]]
) {

  // return a coordinate grid which wraps the STRTree and
  // also makes it easy to print the grid as a CSV

  def getGridId(x: Double, y: Double): Either[Error, String] =
    CoordinateGrid2.getGridId(this.lookup, this.geometryFactory)(x, y)
}

object CoordinateGrid2 extends LazyLogging {

  object TableDefaults {
    def IdColumnName       = "grid_id"
    def GeometryColumnName = "polygon"
  }

  def fromCsv(
    tableFile: File,
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    srid: Int,
    idCol: String = TableDefaults.IdColumnName,
    geoCol: String = TableDefaults.GeometryColumnName
  ): Either[Error, CoordinateGrid2] = {
    val pm: PrecisionModel   = new PrecisionModel()
    val gf: GeometryFactory  = new GeometryFactory(pm, srid)
    val wktReader: WKTReader = new WKTReader(gf)

    implicit val gd: CellDecoder[Polygon] = CellDecoder.from { string =>
      Try { wktReader.read(string).asInstanceOf[Polygon] }.toEither.left.map { t =>
        DecodeError.TypeError(s"unable to decode geometry from WKT: ${t.getMessage}")
      }
    }
    case class Row(grid_id: String, polygon: Polygon) {
      def toTuple: (String, Polygon) = (grid_id, polygon)
    }
    implicit val hd: HeaderDecoder[Row] = HeaderDecoder.decoder(idCol, geoCol) { Row.apply }

    tableFile
      .readCsv[List, Row](rfc.withHeader)
      .sequence
      .left
      .map { t => new Error("failure reading grid table", t) }
      .flatMap { table =>
        logger.info(f"read CSV grid source with ${table.length} rows")
        // lazy, i just don't want to dig out MathTransform and CRS.decode right now
        if (srid != 3857) Left(new Error("currently only supports EPSG:3857 grids"))
        else {
          // polygons are expected to be annotated with their grid id
          val tableWithAnnotations = for { Row(gridId, polygon) <- table } { polygon.setUserData(gridId) }
          build(table.map(_.toTuple), rn, gf)
        }
      }

  }

  /**
    * build a CoordinateGrid2 that fits to the provided bounds and where
    * the grid cells are squares with area
    * gridCellSideLength * gridCellSideLength
    *
    * @param minX the bbox min x value
    * @param maxX the bbox max x value
    * @param minY the bbox min y value
    * @param maxY the bbox max y value
    * @param gridCellSideLength square side length in coordinate system
    * @param srid coordinate system
    * @return a CoordinateGrid2 or an error
    */
  def apply(
    minX: Double,
    maxX: Double,
    minY: Double,
    maxY: Double,
    gridCellSideLength: Double,
    srid: Int,
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]
  ): Either[Error, CoordinateGrid2] = {
    if (minX >= maxX) {
      Left(new Error(s"invalid min, max x values: $minX $maxX"))
    } else if (minY >= maxY) {
      Left(new Error(s"invalid min, max y values: $minY $maxY"))
    } else {

      val initialGrid = Try {
        val gf = new GeometryFactory(new PrecisionModel(), srid)

        // steps between cells in the underlying coordinate system
        val xSteps: Int = math.ceil((maxX - minX) / gridCellSideLength).toInt
        val ySteps: Int = math.ceil((maxY - minY) / gridCellSideLength).toInt

        val gridCells = for {
          x <- 0 until xSteps
          y <- 0 until ySteps
        } yield {
          val xOrigin = minX + (x * gridCellSideLength)
          val yOrigin = minY + (y * gridCellSideLength)
          val lLeft   = new Coordinate(xOrigin, yOrigin)
          val uLeft   = new Coordinate(xOrigin, yOrigin + gridCellSideLength)
          val uRight  = new Coordinate(xOrigin + gridCellSideLength, yOrigin + gridCellSideLength)
          val lRight  = new Coordinate(xOrigin + gridCellSideLength, yOrigin)
          val polygon = gf.createPolygon(Array(lLeft, uLeft, uRight, lRight, lLeft))
          polygon.setSRID(srid)
          val gridId = createGridId(x, y)
          polygon.setUserData(gridId)
          gridId -> polygon
        }

        build(gridCells, rn, gf)

      }.toEither.left.map { t => new Error("failed creating new coordinate grid", t) }

      initialGrid.flatten
    }
  }

  def build(
    gridCells: Seq[(String, Polygon)],
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    gf: GeometryFactory
  ): Either[Error, CoordinateGrid2] = {
    logger.info(f"building CoordinateGrid2 from ${gridCells.length} grid cells")
    Try {
      val tree = new STRtree()

      for {
        (gridId, polygon) <- gridCells
      } {
        tree.insert(polygon.getEnvelopeInternal, polygon) // side-effect
      }

      val gridCellsLookup: Map[String, Polygon] = gridCells.toMap

      val lookupQueries = for {
        edgeTriplet <- rn.edgeTriplets.unsafeRunSync
        src         <- rn.vertex(edgeTriplet.src).unsafeRunSync
        // 2023-13-17: this should match the way getId is invoked, and that seems to
        //             get called with the source coordinate as its argument.
        // dst         <- rn.vertex(edgeTriplet.dst).unsafeRunSync
        // midpoint = LocalAdjacencyListFlowNetwork.midpoint(src.attribute, dst.attribute)
      } yield (edgeTriplet.edgeId, src.attribute)

      val gridIdFn: (Double, Double) => Option[String] =
        (x, y) => {
          CoordinateGrid2.getGridId(tree, gf)(x, y).toOption
        }

      // find the grid id for every edge that intersects with a grid cell
      val edgeLookup = lookupQueries
        .flatMap {
          case (edgeId, midpoint) =>
            gridIdFn(midpoint.x, midpoint.y).map { batchId => (batchId, edgeId) }
        }
        .groupBy { case (batchId, _) => batchId }
        .map {
          case (batchId, grouped) =>
            val (_, edgeIds) = grouped.unzip
            batchId -> edgeIds
        }
      val edgeLookupComplete = gridCells.foldLeft(edgeLookup) {
        case (lookup, (batchId, _)) =>
          lookup.get(batchId) match {
            case None    => lookup.updated(batchId, List.empty)
            case Some(_) => lookup
          }
      }

      new CoordinateGrid2(gridCellsLookup, gf, tree, edgeLookupComplete)
    }.toEither.left.map { t => new Error("failed building coordinate grid", t) }
  }

  /**
    * builds a label for a grid cell id
    * @param x the x step position
    * @param y the y step position
    * @return a grid cell id
    */
  def createGridId(x: Int, y: Int): String = s"$x#$y"

  def getGridId(tree: STRtree, gf: GeometryFactory)(x: Double, y: Double): Either[Error, String] = {
    val result: Try[String] = for {
      queryPoint  <- Try { gf.createPoint(new Coordinate(x, y)) }
      queryResult <- Try { tree.query(queryPoint.getEnvelopeInternal).asScala.head }
      gridId      <- Try { queryResult.asInstanceOf[Polygon].getUserData.asInstanceOf[String] }
    } yield {
      gridId
    }

    result.toEither.left.map { t => new Error(s"failure getting grid id for point ($x, $y)", t) }
  }

  /**
    * inner data structure holding info about a grid cell
    * @param polygon the polygon of a grid cell
    * @param xStep the x position of a grid cell
    * @param yStep the y position of a grid cell
    */
  final case class GridCell(polygon: Polygon, xStep: Int, yStep: Int)

}

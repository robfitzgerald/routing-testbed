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

/**
  * revised coordinate grid which uses JTS polygons + spatial tree search
  * which also can be printed to a .csv with columns grid_id,wkt (in WGS84)
  * so we can overlay the grid on top of the road network for visualizations
  */
final class CoordinateGrid2(
  val gridCells: Map[String, CoordinateGrid2.GridCell],
  val xSteps: Int,
  val ySteps: Int,
  geometryFactory: GeometryFactory,
  private val lookup: STRtree,
  val edgeLookup: Map[String, List[EdgeId]]
) {

  // return a coordinate grid which wraps the STRTree and
  // also makes it easy to print the grid as a CSV

  def getGridId(x: Double, y: Double): Either[Error, String] =
    CoordinateGrid2.getGridId(this.lookup, this.geometryFactory)(x, y)

  /**
    * pretty prints the batch information in a grid
    * @param countsByBatchId grid ids and the counts of requests associated with each
    * @return a pretty-printed grid of this info
    */
  def prettyPrintBatches(countsByBatchId: List[(String, Int)]): String =
    CoordinateGrid2PrintOps.printGrid(countsByBatchId, this.xSteps, GroupIdOrdering)

  /**
    * Ordering used for pretty printing a grid based on the stringified grid indices
    */
  val GroupIdOrdering: Ordering[String] = Ordering.by {
    case CoordinateGrid2PrintOps.GridIdRegex(xStr, yStr) =>
      Try {
        val (xId, yId) = (xStr.toInt, yStr.toInt)
        -(xId + yId * xSteps)
      }.toOption match {
        case None      => Int.MaxValue
        case Some(ord) => ord
      }
    case _ => Int.MaxValue
  }
}

object CoordinateGrid2 {

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

        val strTree: STRtree = new STRtree()

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
          strTree.insert(polygon.getEnvelopeInternal, polygon) // side-effect
          val gridCell = GridCell(polygon, x, y)
          gridId -> gridCell
        }
        val gridCellsLookup: Map[String, GridCell] = gridCells.toMap

        val lookupQueries = for {
          edgeTriplet <- rn.edgeTriplets.unsafeRunSync
          src         <- rn.vertex(edgeTriplet.src).unsafeRunSync
          dst         <- rn.vertex(edgeTriplet.dst).unsafeRunSync
          midpoint = LocalAdjacencyListFlowNetwork.midpoint(src.attribute, dst.attribute)
        } yield (edgeTriplet.edgeId, midpoint)

        val gridIdFn: (Double, Double) => Either[Error, String] = CoordinateGrid2.getGridId(strTree, gf)
        val lookupResult = lookupQueries
          .traverse {
            case (edgeId, midpoint) =>
              gridIdFn(midpoint.x, midpoint.y).map { batchId => (batchId, edgeId) }
          }
          .map {
            _.groupBy { case (batchId, _) => batchId }
              .map {
                case (batchId, grouped) =>
                  val (_, edgeIds) = grouped.unzip
                  batchId -> edgeIds
              }
          }

        lookupResult.map { edgeLookup => new CoordinateGrid2(gridCellsLookup, xSteps, ySteps, gf, strTree, edgeLookup) }
      }.toEither.left.map { t => new Error("failed building coordinate grid", t) }

      initialGrid.flatten
    }
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

    result.toEither.left.map { t => new Error(s"failure getting grid idea for point ($x, $y)", t) }
  }

  /**
    * inner data structure holding info about a grid cell
    * @param polygon the polygon of a grid cell
    * @param xStep the x position of a grid cell
    * @param yStep the y position of a grid cell
    */
  final case class GridCell(polygon: Polygon, xStep: Int, yStep: Int)

}

package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig

import java.io.File
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.algorithm.grid.CoordinateGrid2
import cats.effect.IO
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR

sealed trait GridConfig

object GridConfig {

  final case class CoordinateGridConfig(
    minX: Double,
    maxX: Double,
    minY: Double,
    maxY: Double,
    gridCellSideLength: Double,
    srid: Int
  ) extends GridConfig

  final case class CsvGridConfig(
    file: File,
    srid: Int,
    idColumnName: Option[String],
    geometryColumnName: Option[String]
  ) extends GridConfig

  implicit class GridConfigExtension(gf: GridConfig) {

    def build(rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]): Either[Error, CoordinateGrid2] =
      gf match {
        case cgc: CoordinateGridConfig =>
          CoordinateGrid2(cgc.minX, cgc.maxX, cgc.minY, cgc.maxY, cgc.gridCellSideLength, cgc.srid, rn)
        case t: CsvGridConfig =>
          CoordinateGrid2.fromCsv(
            t.file,
            rn,
            t.srid,
            t.idColumnName.getOrElse(CoordinateGrid2.TableDefaults.IdColumnName),
            t.geometryColumnName.getOrElse(CoordinateGrid2.TableDefaults.GeometryColumnName)
          )
      }
  }

}

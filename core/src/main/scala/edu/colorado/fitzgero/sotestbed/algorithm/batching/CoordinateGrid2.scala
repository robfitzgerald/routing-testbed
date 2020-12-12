package edu.colorado.fitzgero.sotestbed.algorithm.batching

/**
  * revised coordinate grid which uses JTS polygons + spatial tree search
  * which also can be printed to a .csv with columns grid_id,wkt (in WGS84)
  * so we can overlay the grid on top of the road network for visualizations
  */
case class CoordinateGrid2()

object CoordinateGrid2 {

  def apply(
    minX: Double,
    maxX: Double,
    minY: Double,
    maxY: Double,
    splitFactor: Int
  ): Either[Error, CoordinateGrid2] = {
    if (minX >= maxX) {
      Left(new Error(s"invalid min, max x values: $minX $maxX"))
    } else if (minY >= maxY) {
      Left(new Error(s"invalid min, max y values: $minY $maxY"))
    } else {
      val xStep: Double = (maxX - minX) / splitFactor
      val yStep: Double = (maxY - minY) / splitFactor

      // reproduce this logic but to create square polygons
      // val xId = math.min(((x - minX) / xStep).toInt, splitFactor - 1)
      // val yId = math.min(((y - minY) / yStep).toInt, splitFactor - 1)

      // construct spatial tree search

      ???
    }
  }
}

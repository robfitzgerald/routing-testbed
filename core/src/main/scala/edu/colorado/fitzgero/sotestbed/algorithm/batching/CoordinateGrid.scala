package edu.colorado.fitzgero.sotestbed.algorithm.batching

import scala.util.Try
import scala.util.matching.Regex

/**
  * creates a simple grid from a bounding box. given an arbirary point within the box, provides a unique grouping
  * id related to the cell which contains the point, allowing for agent grouping by cell.
  *
  * @param minX the bounding box's minimum x position
  * @param maxX the bounding box's maximum x position
  * @param minY the bounding box's minimum y position
  * @param maxY the bounding box's maximum y position
  * @param splitFactor how many cell to split the space in each dimension; i.e., splitFactor == 3 -> 9 cells
  */
class CoordinateGrid(
  minX: Double,
  maxX: Double,
  minY: Double,
  maxY: Double,
  splitFactor: Int
) {

  // step size between cells in the underlying coordinate space
  val xStep: Double = (maxX - minX) / splitFactor
  val yStep: Double = (maxY - minY) / splitFactor

  /**
    * gives cell ids which start from zero along each axis and end at splitFactor - 1
    *
    * @param x test x value
    * @param y test y value
    * @return an id, in the form "xId#yId"
    */
  def getGridId(x: Double, y: Double): String = {
    val xId = math.min(((x - minX) / xStep).toInt, splitFactor - 1)
    val yId = math.min(((y - minY) / yStep).toInt, splitFactor - 1)
    s"$xId#$yId"
  }

  /**
    * for pretty-print logging of group data
    * @param gridId stringified grid id
    * @return stringified coordinate, or nothing if invalid
    */
  def getGridCoord(gridId: String): Option[String] = gridId match {
    case CoordinateGrid.GridIdRegex(xStr, yStr) =>
      Try {
        val (xId, yId) = (xStr.toInt, yStr.toInt)
        if (xId >= splitFactor || yId >= splitFactor) {
          throw new IndexOutOfBoundsException()
        } else {
          val (x, y) = (minX + (xId * xStep), minY + (yId * yStep))
          f"($x%.4f,$y%.4f)"
        }
      }.toOption
    case _ => None
  }

  /**
    * Ordering used for pretty printing a grid based on the stringified grid indices
    */
  val GroupIdOrdering: Ordering[String] = Ordering.by {
    case CoordinateGrid.GridIdRegex(xStr, yStr) =>
      Try {
        val (xId, yId) = (xStr.toInt, yStr.toInt)
        -(xId + yId * splitFactor)
      }.toOption match {
        case None      => Int.MaxValue
        case Some(ord) => ord
      }
    case _ => Int.MaxValue
  }

  /**
    * presents a spatial grid and grid counts as a string
    * @param grouped the current batch grouping
    * @return a pretty-printed representation of the batch grouping
    */
  def printGrid(grouped: Map[String, List[(String, AgentBatchData)]]): String = {
    grouped
      .map { case (gridId, agents) => (gridId, agents.size) }
      .toList
      .sortBy { _._1 }(GroupIdOrdering)
      .sliding(this.splitFactor, this.splitFactor)
      .map { row => row.map { case (_, count) => count.toString.padTo(3, ' ') }.mkString("") }
      .mkString("\n")
  }

  def printList(grouped: Map[String, List[(String, AgentBatchData)]]): String = {
    val columns: Int = 8
    val asStrings: List[String] = grouped.toList
      .sortBy { case (gridId, _) => gridId }
      .map { case (gridId, agents) => s"$gridId: ${agents.size.toString.padTo(3, ' ')}" }
    val splitAcrossRows: Iterator[String] = for {
      row <- asStrings.sliding(columns, columns)
    } yield {
      row.mkString(" ")
    }
    splitAcrossRows.mkString("\n")
  }
}

object CoordinateGrid {
  val GridIdRegex: Regex = """^(\d+)#(\d+)$""".r

}

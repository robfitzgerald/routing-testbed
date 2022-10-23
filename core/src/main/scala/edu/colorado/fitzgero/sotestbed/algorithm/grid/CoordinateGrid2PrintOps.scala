package edu.colorado.fitzgero.sotestbed.algorithm.grid

import java.io.File

import scala.util.Try
import scala.util.matching.Regex

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData
import kantan.csv._
import kantan.csv.ops._

object CoordinateGrid2PrintOps {
  val GridIdRegex: Regex = """^(\d+)#(\d+)$""".r

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

  /**
    * write the CoordinateGrid2 as a CSV file
    *
    * @param coordinateGrid2 the grid to write
    * @param file            destination file
    * @return nothing if successful, or, an error
    */
  def writeGridToCsv(coordinateGrid2: CoordinateGrid2, file: File): Either[Error, Unit] = {
    val outputData = coordinateGrid2.gridCells.map {
      case (gridCellId, polygon) =>
        (gridCellId, polygon.toString)
    }

    Try {
      file.writeCsv(outputData, rfc.withHeader("grid_id", "polygon"))
    }.toEither.left.map { t => new Error(s"failed writing coordinate grid to CSV output", t) }
  }
}

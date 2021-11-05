package edu.colorado.fitzgero.sotestbed.matsim.analysis

import java.time.LocalTime

import scala.util.Try

import kantan.csv._

case class BatchDataRow(
  time: LocalTime,
  batch_id: String,
  batch_size: Double,
  avgAltPathsPerAgent: Double,
  searchSpaceSamples: BigDecimal,
  searchSpaceSize: BigDecimal,
  searchSpaceExplored: Percent,
  soAssignmentPercent: Percent
)

object BatchDataRow {

  /**
    * combine rows. rows are assumed to share the same time / be part of the same
    * time horizon.
    *
    * @param batchDataRows the rows
    * @return the combination of rows into a single row for this time horizon
    */
  def combine(batchDataRows: List[BatchDataRow]): BatchDataRow = {
    batchDataRows match {
      case Nil              => throw new IllegalArgumentException("called combine on empty list of batch data rows")
      case singleRow :: Nil => singleRow
      case initial :: remainingRows =>
        val initialAccumulator = (initial, 1)
        val (accRow, rowCount) = remainingRows.foldLeft(initialAccumulator) { (acc, thatRow) =>
          val (thisRow, count) = acc
          val updatedRow = thisRow.copy(
            time = thisRow.time,
            batch_id = s"${thisRow.batch_id}-${thatRow.batch_id}",
            batch_size = thisRow.batch_size + thatRow.batch_size,
            avgAltPathsPerAgent = thisRow.avgAltPathsPerAgent + thatRow.avgAltPathsPerAgent,
            searchSpaceSamples = thisRow.searchSpaceSamples + thatRow.searchSpaceSamples,
            searchSpaceSize = thisRow.searchSpaceSize + thatRow.searchSpaceSize,
            searchSpaceExplored = Percent(thisRow.searchSpaceExplored.value + thatRow.searchSpaceExplored.value),
            soAssignmentPercent = Percent(thisRow.soAssignmentPercent.value + thatRow.soAssignmentPercent.value)
          )
          (updatedRow, count + 1)
        }

        val result = accRow.copy(
          batch_size = accRow.batch_size / rowCount,
          avgAltPathsPerAgent = accRow.avgAltPathsPerAgent / rowCount,
          searchSpaceExplored = Percent(accRow.searchSpaceExplored.value / rowCount),
          soAssignmentPercent = Percent(accRow.soAssignmentPercent.value / rowCount)
        )

        result
    }
  }

  implicit val timeCellDecoder: CellDecoder[LocalTime] =
    CellDecoder.from { timeString =>
      Try { LocalTime.parse(timeString) }.toEither.left.map { t => DecodeError.TypeError(t) }
    }

  implicit val percentDecoder: CellDecoder[Percent] = Percent.cellDecoder

  val headerDecoder: HeaderDecoder[BatchDataRow] =
    HeaderDecoder.decoder(
      "time",
      "batch_id",
      "batch_size",
      "avgAltPathsPerAgent",
      "searchSpaceSamples",
      "searchSpaceSize",
      "searchSpaceExplored",
      "soAssignmentPercent"
    ) { BatchDataRow.apply }
}

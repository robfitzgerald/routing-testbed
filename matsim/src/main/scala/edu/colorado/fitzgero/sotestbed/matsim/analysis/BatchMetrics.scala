package edu.colorado.fitzgero.sotestbed.matsim.analysis

import java.io.File
import kantan.csv._
import kantan.csv.ops._

case class BatchMetrics(
  batchSize: Double = 0,
  altPaths: Double = 0,
  searchSpaceSamples: BigDecimal = 0,
  searchSpaceSize: BigDecimal = 0,
  searchSpaceExplored: Double = 0,
  soAssignmentPercent: Double = 0,
  observations: Int = 0
) {

  def add(row: BatchDataRow): BatchMetrics = {
    this.copy(
      batchSize = this.batchSize + row.batch_size,
      altPaths = this.altPaths + row.avgAltPathsPerAgent,
      searchSpaceSamples = this.searchSpaceSamples + row.searchSpaceSamples,
      searchSpaceSize = this.searchSpaceSize + row.searchSpaceSize,
      searchSpaceExplored = this.searchSpaceExplored + row.searchSpaceExplored.value,
      soAssignmentPercent = this.soAssignmentPercent + row.soAssignmentPercent.value,
      observations = this.observations + 1
    )
  }

  def add(that: BatchMetrics): BatchMetrics = {
    this.copy(
      batchSize = this.batchSize + that.batchSize,
      altPaths = this.altPaths + that.altPaths,
      searchSpaceSamples = this.searchSpaceSamples + that.searchSpaceSamples,
      searchSpaceSize = this.searchSpaceSize + that.searchSpaceSize,
      searchSpaceExplored = this.searchSpaceExplored + that.searchSpaceExplored,
      soAssignmentPercent = this.soAssignmentPercent + that.soAssignmentPercent,
      observations = this.observations + that.observations
    )
  }

  override def toString: String = {
    if (observations == 0) {
      "0,0,0,0,0,0,0"
    } else {
      f"${batchSize / observations}%.2f,${altPaths / observations}%.2f,${searchSpaceSamples / observations}%.2f" +
        f",${searchSpaceSize / observations}%.2f,${(searchSpaceExplored / observations) * 100.0}%.2f%%," +
        f"${(soAssignmentPercent / observations) * 100.0}%.2f%%,$observations"
    }
  }
}

object BatchMetrics {

  val Header: String = "batchSize,altPaths,searchSpaceSamples,searchSpaceSize,searchSpaceExplored,soAssignmentPercent,observations"

  def fromFile(file: File): Either[ReadError, BatchMetrics] = {
    implicit val hd: HeaderDecoder[BatchDataRow] = BatchDataRow.headerDecoder

    val result: Either[ReadError, BatchMetrics] = for {
      rows <- ReadResult.sequence(file.readCsv[List, BatchDataRow](rfc.withHeader))
    } yield {
      rows.foldLeft(BatchMetrics()) { _.add(_) }
    }

    result
  }
}

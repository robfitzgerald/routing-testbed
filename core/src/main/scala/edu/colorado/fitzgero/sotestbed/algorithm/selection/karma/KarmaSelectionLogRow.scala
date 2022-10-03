package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import kantan.csv.HeaderDecoder

final case class KarmaSelectionLogRow(
  batchId: String,
  agentId: String,
  requestTime: Long,
  startBalance: Karma,
  endBalance: Karma,
  bidValue: Karma,
  route: Int,
  requestTimeEstimateSeconds: Long,
  afterSelectionEstimateSeconds: Long
) {

  def toLogRow: String =
    s"$batchId,$agentId,$requestTime,$startBalance,$endBalance,$bidValue,$route,$requestTimeEstimateSeconds,$afterSelectionEstimateSeconds"
}

object KarmaSelectionLogRow {

  val KarmaLogFilename = "karma_log.csv"

  val KarmaLogHeader: String =
    "batchId,agentId,requestTime,startKarma,endKarma,bidValue,selectedRoute,requestTimeEstimateSeconds,afterSelectionEstimateSeconds"

  implicit val dec: HeaderDecoder[KarmaSelectionLogRow] =
    HeaderDecoder.decoder(
      "batchId",
      "agentId",
      "requestTime",
      "startBalance",
      "endBalance",
      "bidValue",
      "route",
      "requestTimeEstimateSeconds",
      "afterSelectionEstimateSeconds"
    ) { KarmaSelectionLogRow.apply }

}

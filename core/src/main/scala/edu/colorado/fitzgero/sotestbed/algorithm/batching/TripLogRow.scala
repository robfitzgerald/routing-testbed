package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.numeric.Meters

import kantan.csv._

final case class TripLogRow(
  agentId: String,
  departureTime: SimTime,
  arrivalTime: SimTime,
  originalTravelTimeEstimate: SimTime,
  finalTravelTime: SimTime,
  // originalDistanceEstimate: Meters,
  finalDistance: Meters
) {

  def travelTimeDiff: SimTime = this.originalTravelTimeEstimate - this.finalTravelTime

  /**
    * writes the row to a string intended for a CSV. i make the horrible assumption here
    * that SimTime.toString has the same result as CellEncoder[SimTime] in kantan. see
    * SimTime.scala to confirm if things get wonky.
    *
    * @return stringified trip log row
    */
  override def toString =
    f"$agentId,$departureTime,$arrivalTime,$originalTravelTimeEstimate," +
      s"$finalTravelTime,$finalDistance"
}

object TripLogRow {

  def apply(
    row: AgentBatchData.SOAgentArrivalData,
    originalTravelTimeEstimate: SimTime
    // originalDistanceEstimate: Meters
  ): TripLogRow =
    TripLogRow(
      agentId = row.agentId,
      departureTime = row.departureTime,
      arrivalTime = row.arrivalTime,
      originalTravelTimeEstimate = originalTravelTimeEstimate,
      finalTravelTime = row.finalTravelTime,
      finalDistance = row.finalDistance
    )

  val Columns = List(
    "agentId",
    "departureTime",
    "arrivalTime",
    "originalTravelTimeEstimate",
    "finalTravelTime",
    "finalDistance"
  )
  def Header = Columns.toList.mkString(",")

  def dec: HeaderDecoder[TripLogRow] =
    HeaderDecoder.decoder(
      "agentId",
      "departureTime",
      "arrivalTime",
      "originalTravelTimeEstimate",
      "finalTravelTime",
      "finalDistance"
    ) { TripLogRow.apply }

  def enc: HeaderEncoder[TripLogRow] =
    HeaderEncoder.encoder(
      "agentId",
      "departureTime",
      "arrivalTime",
      "originalTravelTimeEstimate",
      "finalTravelTime",
      "finalDistance"
    ) { row =>
      (
        row.agentId,
        row.departureTime.toString,
        row.arrivalTime.toString,
        row.originalTravelTimeEstimate.toString,
        row.finalTravelTime.toString,
        row.finalDistance.value.toString
      )
    }

  implicit val codec: HeaderCodec[TripLogRow] = HeaderCodec.from(dec)(enc)

}

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
  finalDistance: Meters,
  replannings: Int
) {

  /**
    * difference in travel time from the original estimate and the final (experienced)
    * travel time. also referred to as the inverse delay, this is computed as o - f,
    * where the resulting value is negative if there is delay, positive if they arrive
    * ahead of the estimate.
    */
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
      s"$finalTravelTime,$finalDistance,$replannings"
}

object TripLogRow {

  def apply(
    row: AgentBatchData.SOAgentArrivalData,
    originalTravelTimeEstimate: SimTime,
    replannings: Int
    // originalDistanceEstimate: Meters
  ): TripLogRow =
    TripLogRow(
      agentId = row.agentId,
      departureTime = row.departureTime,
      arrivalTime = row.arrivalTime,
      originalTravelTimeEstimate = originalTravelTimeEstimate,
      finalTravelTime = row.finalTravelTime,
      finalDistance = row.finalDistance,
      replannings = replannings
    )

  val Columns = List(
    "agentId",
    "departureTime",
    "arrivalTime",
    "originalTravelTimeEstimate",
    "finalTravelTime",
    "finalDistance",
    "replannings"
  )
  def Header = Columns.toList.mkString(",")

  def dec: HeaderDecoder[TripLogRow] =
    HeaderDecoder.decoder(
      "agentId",
      "departureTime",
      "arrivalTime",
      "originalTravelTimeEstimate",
      "finalTravelTime",
      "finalDistance",
      "replannings"
    ) { TripLogRow.apply }

  def enc: HeaderEncoder[TripLogRow] =
    HeaderEncoder.encoder(
      "agentId",
      "departureTime",
      "arrivalTime",
      "originalTravelTimeEstimate",
      "finalTravelTime",
      "finalDistance",
      "replannings"
    ) { row =>
      (
        row.agentId,
        row.departureTime.toString,
        row.arrivalTime.toString,
        row.originalTravelTimeEstimate.toString,
        row.finalTravelTime.toString,
        row.finalDistance.value.toString,
        row.replannings.toString
      )
    }

  implicit val codec: HeaderCodec[TripLogRow] = HeaderCodec.from(dec)(enc)

}

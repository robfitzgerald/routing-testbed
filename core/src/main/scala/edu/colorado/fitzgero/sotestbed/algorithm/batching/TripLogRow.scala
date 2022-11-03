package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.numeric.Meters

import kantan.csv._
import edu.colorado.fitzgero.sotestbed.model.agent.RequestClass

final case class TripLogRow(
  agentId: String,
  requestClass: RequestClass,
  departureTime: SimTime,
  arrivalTime: SimTime,
  originalTravelTimeEstimate: SimTime,
  finalTravelTime: SimTime,
  freeFlowTravelTime: SimTime,
  finalDistance: Meters,
  replannings: Int,
  uoRoutesAssigned: Int
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
    f"$agentId,$requestClass,$departureTime,$arrivalTime,$originalTravelTimeEstimate," +
      s"$finalTravelTime,$freeFlowTravelTime,$finalDistance,$replannings,$uoRoutesAssigned"
}

object TripLogRow {

  def apply(
    row: AgentBatchData.SOAgentArrivalData,
    originalTravelTimeEstimate: SimTime,
    freeFlowTravelTime: SimTime,
    replannings: Int,
    uoRoutesAssigned: Int
    // originalDistanceEstimate: Meters
  ): TripLogRow =
    TripLogRow(
      agentId = row.agentId,
      requestClass = row.requestClass,
      departureTime = row.departureTime,
      arrivalTime = row.arrivalTime,
      originalTravelTimeEstimate = originalTravelTimeEstimate,
      finalTravelTime = row.finalTravelTime,
      freeFlowTravelTime = freeFlowTravelTime,
      finalDistance = row.finalDistance,
      replannings = replannings,
      uoRoutesAssigned = uoRoutesAssigned
    )

  val Columns = List(
    "agentId",
    "requestClass",
    "departureTime",
    "arrivalTime",
    "originalTravelTimeEstimate",
    "finalTravelTime",
    "freeFlowTravelTime",
    "finalDistance",
    "replannings",
    "uoRoutesAssigned"
  )
  def Header = Columns.toList.mkString(",")

  def dec: HeaderDecoder[TripLogRow] =
    HeaderDecoder.decoder(
      "agentId",
      "requestClass",
      "departureTime",
      "arrivalTime",
      "originalTravelTimeEstimate",
      "finalTravelTime",
      "freeFlowTravelTime",
      "finalDistance",
      "replannings",
      "uoRoutesAssigned"
    ) { TripLogRow.apply }

  def enc: HeaderEncoder[TripLogRow] =
    HeaderEncoder.encoder(
      "agentId",
      "requestClass",
      "departureTime",
      "arrivalTime",
      "originalTravelTimeEstimate",
      "finalTravelTime",
      "freeFlowTravelTime",
      "finalDistance",
      "replannings",
      "uoRoutesAssigned"
    ) { row =>
      (
        row.agentId,
        row.requestClass,
        row.departureTime.toString,
        row.arrivalTime.toString,
        row.originalTravelTimeEstimate.toString,
        row.finalTravelTime.toString,
        row.freeFlowTravelTime.toString,
        row.finalDistance.value.toString,
        row.replannings.toString,
        row.uoRoutesAssigned.toString
      )
    }

  implicit val codec: HeaderCodec[TripLogRow] = HeaderCodec.from(dec)(enc)

}

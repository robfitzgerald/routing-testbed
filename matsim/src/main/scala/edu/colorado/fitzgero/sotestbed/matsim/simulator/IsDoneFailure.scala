package edu.colorado.fitzgero.sotestbed.matsim.simulator

sealed trait IsDoneFailure

object IsDoneFailure {
  final case class TimeoutFailure(message: String) extends Error with IsDoneFailure
  final case class FinishingError(message: String) extends Error with IsDoneFailure
}

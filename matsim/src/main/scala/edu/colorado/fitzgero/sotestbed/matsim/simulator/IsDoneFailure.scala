package edu.colorado.fitzgero.sotestbed.matsim.simulator

sealed trait IsDoneFailure

object IsDoneFailure {
  final case class TimeoutFailure(message: String) extends IsDoneFailure
}
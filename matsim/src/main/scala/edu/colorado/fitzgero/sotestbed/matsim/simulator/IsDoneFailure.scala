package edu.colorado.fitzgero.sotestbed.matsim.simulator

sealed trait IsDoneFailure extends Throwable with Product with Serializable

object IsDoneFailure {
  final case class TimeoutFailure(message: String) extends Error(message) with IsDoneFailure

  final case class FinishingError(message: String) extends Error(message) with IsDoneFailure

  object TimeoutFailure {

    def apply(msg: String, t: Throwable): TimeoutFailure = {
      val error = TimeoutFailure(msg)
      error.initCause(t)
      error
    }
  }

  object FinishingError {

    def apply(msg: String, t: Throwable): FinishingError = {
      val error = FinishingError(msg)
      error.initCause(t)
      error
    }
  }
}

package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.CongestionObservationType.CongestionObservationResult

sealed trait CongestionTransformType

object CongestionTransformType {

  /**
    * transforms the output of an observation using tanh
    * @param xFactor factor to multiply x with inside the tanh function
    */
  case class TanHTransform(xFactor: Double = 1.0) extends CongestionTransformType

  implicit class CongestionTransformExtensions(t: CongestionTransformType) {

    /**
      * applies a transform to the accumulated increased travel time value
      */
    def applyTransform(obs: CongestionObservationResult): CongestionObservationResult =
      t match {
        case TanHTransform(xFactor) =>
          val x                   = obs.increaseAccumulated * xFactor
          val transformedIncrease = math.tanh(x)
          obs.copy(increaseAccumulated = transformedIncrease)
      }
  }
}

package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy

sealed trait ObservationAggregation

object ObservationAggregation {

  case object Average extends ObservationAggregation

  implicit class OAExtensions(oa: ObservationAggregation) {

    def aggregate(obs: List[Double]): Double = oa match {
      case Average => if (obs.isEmpty) 0.0 else obs.sum
    }

  }

}

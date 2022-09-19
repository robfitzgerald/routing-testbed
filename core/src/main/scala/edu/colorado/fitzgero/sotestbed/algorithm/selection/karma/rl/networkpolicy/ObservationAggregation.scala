package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR

sealed trait ObservationAggregation

object ObservationAggregation {

  case object MeanSpeedDiff             extends ObservationAggregation
  case object DistanceWeightedSpeedDiff extends ObservationAggregation

  implicit class OAExtensions(oa: ObservationAggregation) {

    def aggregate(obs: List[EdgeBPR]): Double = {
      if (obs.isEmpty) 0.0
      else
        oa match {

          case MeanSpeedDiff =>
            val diffs = obs.map(e => e.freeFlowSpeed.value - e.observedSpeed.value)
            diffs.sum

          case DistanceWeightedSpeedDiff =>
            val diffsWeightedSum = obs.map { e =>
              val speed = e.freeFlowSpeed.value - e.observedSpeed.value
              speed * e.distance.value
            }.sum
            val weightSum = obs.map { _.distance.value }.sum
            diffsWeightedSum / weightSum
        }
    }

  }

}

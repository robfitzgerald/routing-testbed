package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.CongestionObservationType.CongestionObservationResult

sealed trait NetworkPolicySignalGenerator

object NetworkPolicySignalGenerator {

  case object UserOptimalGenerator extends NetworkPolicySignalGenerator

  case class ProportionalBernoulliGenerator(rng: Random) extends NetworkPolicySignalGenerator

  case class ScaledBernoulliGenerator(scale: Double, rng: Random) extends NetworkPolicySignalGenerator

  implicit class GeneratorOps(gen: NetworkPolicySignalGenerator) {

    def generateSignal(obs: CongestionObservationResult): NetworkPolicySignal =
      gen match {
        case UserOptimalGenerator =>
          NetworkPolicySignal.UserOptimal
        case ProportionalBernoulliGenerator(rng) =>
          val pct    = math.min(1.0, math.max(0.0, obs.increaseAccumulated))
          val signal = NetworkPolicySignal.ThresholdSampling(pct, rng)
          signal
        case ScaledBernoulliGenerator(scale, rng) =>
          val pct    = math.min(1.0, math.max(0.0, obs.increaseAccumulated * scale))
          val signal = NetworkPolicySignal.ThresholdSampling(pct, rng)
          signal
      }
  }
}

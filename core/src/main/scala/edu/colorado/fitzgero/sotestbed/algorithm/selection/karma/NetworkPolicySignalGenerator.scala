package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.CongestionObservationType.CongestionObservationResult

sealed trait NetworkPolicySignalGenerator

object NetworkPolicySignalGenerator {

  case object UserOptimalGenerator extends NetworkPolicySignalGenerator

  case class RandomGenerator(rng: Random) extends NetworkPolicySignalGenerator

  case class ThresholdSamplingGenerator(rng: Random) extends NetworkPolicySignalGenerator

  case class ScaledThresholdSamplingGenerator(scale: Double, rng: Random) extends NetworkPolicySignalGenerator

  implicit class GeneratorOps(gen: NetworkPolicySignalGenerator) {

    def generateSignal(obs: CongestionObservationResult): NetworkPolicySignal =
      gen match {
        case UserOptimalGenerator =>
          NetworkPolicySignal.UserOptimal
        case RandomGenerator(rng) =>
          val pct    = rng.nextDouble
          val signal = NetworkPolicySignal.ThresholdSampling(pct, rng)
          signal
        case ThresholdSamplingGenerator(rng) =>
          val pct    = math.min(1.0, math.max(0.0, obs.increaseAccumulated))
          val signal = NetworkPolicySignal.ThresholdSampling(pct, rng)
          signal
        case ScaledThresholdSamplingGenerator(scale, rng) =>
          val pct    = math.min(1.0, math.max(0.0, obs.increaseAccumulated * scale))
          val signal = NetworkPolicySignal.ThresholdSampling(pct, rng)
          signal
      }
  }
}

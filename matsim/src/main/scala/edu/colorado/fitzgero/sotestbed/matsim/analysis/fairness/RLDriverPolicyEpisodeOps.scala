package edu.colorado.fitzgero.sotestbed.matsim.analysis.fairness

import cats.effect.IO
import edu.colorado.fitzgero.sotestbed.rllib._
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimRunConfig

object RLDriverPolicyEpisodeOps {

  /**
    * compares the travel time of each trip from a reference run. computes
    * the fairness of the travel time diff using the user-perceived variant of
    * Jain's Fairness Index.
    *
    * @param config
    * @param referenceExperiment
    * @return
    */
  def generateEpisodeReward(config: MATSimRunConfig, referenceExperiment: String = "selfish"): IO[Reward] = {
    ???
  }
}

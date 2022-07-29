package edu.colorado.fitzgero.sotestbed.matsim.analysis.fairness

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.rllib.Reward

class RLDriverPolicyEpisodeOpsTest extends SoTestBedBaseTest {
  "all agents have matching travel time improvement" should {
    "return 0.0 (100%)" in {
      // example 1 scheme 2 from Jain paper, page 6
      // all agents have the same value so it is a perfectly fair result
      val xs = Seq("a", "b", "c").zip(
        Seq(
          17.3,
          17.3,
          17.3
        )
      )

      RLDriverPolicyEpisodeOps
        .generateSingleAgentRewards(xs)
        .foreach {
          _.unzip._2.foreach {
            case r: Reward.SingleAgentReward => r.reward should equal(0.0)
          }
        }
    }
  }
  "called with an unfair distribution" should {
    "return lower values for worse results" in {
      // example 1 scheme 2 from Jain paper, page 6
      // all agents have the same value so it is a perfectly fair result
      val xs = Seq("a", "b", "c").zip(
        Seq(
          -60.0, // 1 minute longer
          -30.0, // 30 seconds longer
          -120.0 // 2 minutes longer
        )
      )

      RLDriverPolicyEpisodeOps.generateSingleAgentRewards(xs).foreach(println)
    }
  }
}

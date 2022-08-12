package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.rllib.Reward

class RLDriverPolicyEpisodeOpsTest extends SoTestBedBaseTest {
  "generateSingleAgentRewards" when {
    "allocations are empty" should {
      "return an empty list" in {
        RLDriverPolicyEpisodeOps
          .generateSingleAgentRewards(List.empty) match {
          case Left(value)  => fail("should not fail", value)
          case Right(value) => value.isEmpty should be(true)
        }

      }
    }
    "all agents have matching travel time improvement" should {
      "return 0.0 (100%)" in {
        // example 1 scheme 2 from Jain paper, page 6
        // all agents have the same value so it is a perfectly fair result
        val xs = List("a", "b", "c").zip(
          List(
            -17.3,
            -17.3,
            -17.3
          )
        )

        RLDriverPolicyEpisodeOps
          .generateSingleAgentRewards(xs)
          .foreach {
            _.unzip._2.foreach {
              case r: Reward.SingleAgentReward => r.reward should equal(0.0)
              case other                       => fail("returned wrong type of reward")
            }
          }
      }
    }
    "all agents have small differences in allocations" should {
      "return something" in {
        val xs = List("a", "b", "c").zip(
          List(
            -17.3,
            -17.2,
            -17.1
          )
        )

        RLDriverPolicyEpisodeOps
          .generateSingleAgentRewards(xs)
          .foreach {
            _.unzip._2.foreach {
              case r: Reward.SingleAgentReward => println(r)
              case other                       => fail("returned wrong type of reward")
            }
          }
      }
    }
    "called with an unfair distribution" should {
      "return lower values for worse results" in {
        val xs = List("a", "b", "c").zip(
          List(
            -60.0, // 1 minute longer
            -30.0, // 30 seconds longer
            -120.0 // 2 minutes longer
          )
        )

        RLDriverPolicyEpisodeOps.generateSingleAgentRewards(xs).foreach(println)
      }
    }
    "called on a distribution that sums to zero" should {
      "not be affected (when using the default allocation transform)" in {
        val xs     = List("a", "b", "c").zip(List(-60.0, 0.0, 60.0))
        val result = RLDriverPolicyEpisodeOps.generateSingleAgentRewards(xs)
        result match {
          case Left(value) => fail("should not have failed", value)
          case Right(rewards) =>
            rewards match {
              case (_, r1) :: (_, r2) :: (_, r3) :: Nil =>
                r1 should equal(Reward.SingleAgentReward(-1.0))
                r2 should equal(Reward.SingleAgentReward(0.0))
                r3 should equal(Reward.SingleAgentReward(0.0))
              case other =>
                fail(s"unexpected result size ${other.size}")
            }
        }
      }
    }
  }
}

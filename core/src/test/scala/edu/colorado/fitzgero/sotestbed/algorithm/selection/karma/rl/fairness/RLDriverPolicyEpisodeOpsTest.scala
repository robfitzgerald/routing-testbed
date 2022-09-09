package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.rllib.Reward

class RLDriverPolicyEpisodeOpsTest extends SoTestBedBaseTest {
  "generateSingleAgentRewards" when {
    "allocations are empty" should {
      "return an empty list" in {
        RLDriverPolicyEpisodeOps
          .generateSingleAgentRewardValues(List.empty) match {
          case Left(value)  => fail("should not fail", value)
          case Right(value) => value.isEmpty should be(true)
        }

      }
    }
    "all agents have matching travel time improvement" should {
      "return 1.0 (100%)" in {
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
          .generateSingleAgentRewardValues(xs)
          .foreach {
            _.unzip._2.foreach {
              _ should equal(1.0)
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
          .generateSingleAgentRewardValues(xs)
          .foreach {
            _.unzip._2.foreach { println }
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

        RLDriverPolicyEpisodeOps.generateSingleAgentRewardValues(xs).foreach(println)
      }
    }
    "called on a distribution that sums to zero" should {
      "not be affected (when using the default allocation transform)" in {
        val xs     = List("a", "b", "c").zip(List(-60.0, 0.0, 60.0))
        val result = RLDriverPolicyEpisodeOps.generateSingleAgentRewardValues(xs)
        result match {
          case Left(value) => fail("should not have failed", value)
          case Right(rewards) =>
            rewards match {
              case (_, r1) :: (_, r2) :: (_, r3) :: Nil =>
                r1 should equal(0.0)
                r2 should equal(1.0)
                r3 should equal(1.0) // value was flattened to zero in transform
              case other =>
                fail(s"unexpected result size ${other.size}")
            }
        }
      }
    }
  }
}

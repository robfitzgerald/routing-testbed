package edu.colorado.fitzgero.sotestbed.rllib

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest._

class PolicyClientRequestTest extends SoTestBedBaseTest {
  "PolicyClientMessage" when {
    "toJson" when {
      "called on a StartEpisodeMessage" should {
        "produce valid json" in {
          val msg: PolicyClientRequest = StartEpisodeRequest(Some(EpisodeId("ep_id")), training_enabled = true)
          val encoded                  = msg.toJson
          encoded.as[PolicyClientRequest] match {
            case Left(error)        => fail(error)
            case Right(codecResult) => codecResult shouldBe a[StartEpisodeRequest]
          }

        }
      }
      "called on a GetActionMessage" should {
        "produce valid json" in {
          val obs     = Observation.MultiAgentObservation(Map(AgentId("bob") -> List(551212.2, 123.4)))
          val msg     = GetActionRequest(EpisodeId("ep_id"), observation = obs)
          val encoded = msg.toJson
          encoded.as[PolicyClientRequest] match {
            case Left(error)        => fail(error)
            case Right(codecResult) => codecResult shouldBe a[GetActionRequest]
          }
        }
      }
      "called on a LogActionMessage" should {
        "produce valid json" in {
          val obs     = Observation.MultiAgentObservation(Map(AgentId("bob") -> List(551212.2, 123.4)))
          val act     = Action.MultiAgentAction(Map(AgentId("bob") -> 3))
          val msg     = LogActionRequest(EpisodeId("ep_id"), observation = obs, action = act)
          val encoded = msg.toJson
          encoded.as[PolicyClientRequest] match {
            case Left(error)        => fail(error)
            case Right(codecResult) => codecResult shouldBe a[LogActionRequest]
          }
        }
      }
      "called on a LogReturns" should {
        "produce valid json" in {
          val msg =
            LogReturnsRequest(EpisodeId("ep_id"), reward = 3.14, info = Map("config" -> "off"), done = Map.empty)
          val encoded = msg.toJson
          encoded.as[PolicyClientRequest] match {
            case Left(error)        => fail(error)
            case Right(codecResult) => codecResult shouldBe a[LogReturnsRequest]
          }
        }
      }
      "called on a LogReturns with an empty info collection" should {
        "produce valid json" in {
          val msg     = LogReturnsRequest(EpisodeId("ep_id"), reward = 3.14, info = Map.empty, done = Map.empty)
          val encoded = msg.toJson
          encoded.as[PolicyClientRequest] match {
            case Left(error)        => fail(error)
            case Right(codecResult) => codecResult shouldBe a[LogReturnsRequest]
          }
        }
      }
      "called on a EndEpisode" should {
        "produce valid json" in {
          val msg     = EndEpisodeRequest(EpisodeId("ep_id"), observation = Observation.MultiAgentObservation(Map.empty))
          val encoded = msg.toJson
          encoded.as[PolicyClientRequest] match {
            case Left(error)        => fail(error)
            case Right(codecResult) => codecResult shouldBe a[EndEpisodeRequest]
          }
        }
      }
    }
  }
}

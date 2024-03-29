package edu.colorado.fitzgero.sotestbed.rllib

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest._
import edu.colorado.fitzgero.sotestbed.rllib.Reward.{MultiAgentReward, SingleAgentReward}

class PolicyClientRequestTest extends SoTestBedBaseTest {
  val mockPrefix = "prefix"
  "PolicyClientMessage" when {
    "toJson" when {
      "called on a StartEpisodeMessage" should {
        "produce valid json" in {
          val msg: PolicyClientRequest =
            StartEpisodeRequest(Some(EpisodeId("ep_id", mockPrefix)), training_enabled = true)
          val encoded = msg.toJson
          encoded.as[PolicyClientRequest] match {
            case Left(error)        => fail(error)
            case Right(codecResult) => codecResult shouldBe a[StartEpisodeRequest]
          }

        }
      }
      "called on a GetActionMessage" should {
        "produce valid json" in {
          val obs     = Observation.MultiAgentObservation(Map(AgentId("bob") -> List(551212.2, 123.4)))
          val msg     = GetActionRequest(EpisodeId("ep_id", mockPrefix), observation = obs)
          val encoded = msg.toJson
          encoded.as[PolicyClientRequest] match {
            case Left(error)        => fail(error)
            case Right(codecResult) => codecResult shouldBe a[GetActionRequest]
          }
        }
      }
      "called on a LogActionMessage with discrete action" should {
        "produce valid json" in {
          val obs     = Observation.MultiAgentObservation(Map(AgentId("bob") -> List(551212.2, 123.4)))
          val act     = Action.MultiAgentDiscreteAction(Map(AgentId("bob") -> 3))
          val msg     = LogActionRequest(EpisodeId("ep_id", mockPrefix), observation = obs, action = act)
          val encoded = msg.toJson
          encoded.as[PolicyClientRequest] match {
            case Left(error)        => fail(error)
            case Right(codecResult) => codecResult shouldBe a[LogActionRequest]
          }
        }
      }
      "called on a LogActionMessage with real action" should {
        "produce valid json" in {
          val obs     = Observation.MultiAgentObservation(Map(AgentId("bob") -> List(551212.2, 123.4)))
          val pi      = 3.14159
          val act     = Action.MultiAgentRealAction(Map(AgentId("bob") -> pi))
          val msg     = LogActionRequest(EpisodeId("ep_id", mockPrefix), observation = obs, action = act)
          val encoded = msg.toJson
          encoded.as[PolicyClientRequest] match {
            case Left(error) => fail(error)
            case Right(codecResult) =>
              codecResult match {
                case LogActionRequest(_, _, action) =>
                  action match {
                    case Action.MultiAgentRealAction(actionValue) =>
                      actionValue.get(AgentId("bob")) should equal(Some(pi))
                    case other => fail(s"wrong action type $other")
                  }
                case other =>
                  fail(s"wrong action type $other")
              }
          }
        }
      }
      "called on a LogReturns" should {
        "produce valid json" in {
          val msg =
            LogReturnsRequest(
              EpisodeId("ep_id", mockPrefix),
              reward = MultiAgentReward(Map(AgentId("Bob") -> 3.14)),
              info = Some(Map("config"                     -> "off")),
              done = None
            )
          val encoded = msg.toJson
          encoded.as[PolicyClientRequest] match {
            case Left(error) =>
              fail(error)
            case Right(codecResult) =>
              codecResult shouldBe a[LogReturnsRequest]
          }
        }
      }
      "called on a LogReturns with an empty info collection" should {
        "produce valid json" in {
          val msg =
            LogReturnsRequest(
              EpisodeId("ep_id", mockPrefix),
              reward = MultiAgentReward(Map(AgentId("Bob") -> 3.14)),
              info = None,
              done = None
            )
          val encoded = msg.toJson
          encoded.as[PolicyClientRequest] match {
            case Left(error) =>
              fail(error)
            case Right(codecResult) =>
              codecResult shouldBe a[LogReturnsRequest]
          }
        }
      }
      "called on a EndEpisode" should {
        "produce valid json" in {
          val msg = EndEpisodeRequest(
            EpisodeId("ep_id", mockPrefix),
            observation = Observation.MultiAgentObservation(Map.empty)
          )
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

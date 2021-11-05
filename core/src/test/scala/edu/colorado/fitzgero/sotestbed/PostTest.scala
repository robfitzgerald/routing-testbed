package edu.colorado.fitzgero.sotestbed

import scala.concurrent.duration._
import scala.language.postfixOps

import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse.GetActionResponse
import edu.colorado.fitzgero.sotestbed.rllib.{Action, AgentId, EpisodeId, Observation, PolicyClientResponse}
import sttp.client3._
//import edu.colorado.fitzgero.sotestbed.proto.v1._
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest._
import io.circe.syntax._
import io.circe.parser._

class PostTest extends SoTestBedBaseTest {
  "POST" ignore {
    "StartEpisodeRequest called" should {
      "result in response" in {

        val msg: PolicyClientRequest =
          StartEpisodeRequest(episode_id = Some(EpisodeId("floof")), training_enabled = true)
        val reqBody = msg.asJson.toString

//        val reqBody = ModelRequest.newBuilder().setX("poop").setY(90124).setZ(8675308).build()

        val request =
          basicRequest
          //            .contentType("application/x-protobuf")
            .body(reqBody.getBytes)
            .response(asByteArray)
            .post(uri"http://localhost:9900")

        println("request prepared")
        println(request)

        val backend = HttpURLConnectionBackend()

        println("backend prepared")

        val response = request.send(backend)

        println("request sent (sync), response received")

        println(response.body)
        println(response.headers)

        response.body match {
          case Left(failureMsg) =>
            fail(failureMsg)
          case Right(responseBody) =>
            val responseString = new String(responseBody)
            println(responseString)
        }
      }
    }
    "GetActionRequest called" should {
      "result in response" in {

        val msg: PolicyClientRequest = GetActionRequest(
          episode_id = EpisodeId("floof"),
          observation = Observation.MultiAgentObservation(
            Map(
              AgentId("group_1") -> List(List(0.0, 0.0, 10.0, 10.0, 0.10), List(-10.0, -10.0, 0.0, 0.0, 0.70)),
              AgentId("group_2") -> List(List(10.0, 10.0, 0.0, 00.0, 0.02), List(0.0, 0.0, -10.0, -10.0, 0.99))
            )
          )
        )
        val reqBody = msg.asJson.toString

        //        val reqBody = ModelRequest.newBuilder().setX("poop").setY(90124).setZ(8675308).build()

        val request =
          basicRequest
          //            .contentType("application/x-protobuf")
            .body(reqBody.getBytes)
            .response(asByteArray)
            .readTimeout(30 minutes)
            .post(uri"http://localhost:9900")

        println("request prepared")
        println(request)

        val backend = HttpURLConnectionBackend()

        println("backend prepared")

        val response = request.send(backend)

        println("request sent (sync), response received")

        println(response.body)
        println(response.headers)

        response.body match {
          case Left(failureMsg) =>
            fail(failureMsg)
          case Right(responseBody) =>
            val responseString = new String(responseBody)
            println(responseString)
        }
      }
    }
    "LogActionRequest called" should {
      "result in response" in {

        val msg: PolicyClientRequest = LogActionRequest(
          episode_id = EpisodeId("floof"),
          observation = Observation.MultiAgentObservation(
            Map(
              AgentId("group_1") -> List(List(0.0, 0.0, 10.0, 10.0, 0.10), List(-10.0, -10.0, 0.0, 0.0, 0.70)),
              AgentId("group_2") -> List(List(10.0, 10.0, 0.0, 00.0, 0.02), List(0.0, 0.0, -10.0, -10.0, 0.99))
            )
          ),
          action = Action.MultiAgentDiscreteAction(
            Map(
              AgentId("group_1") -> List(0, 7),
              AgentId("group_2") -> List(8, 4)
            )
          )
        )
        val reqBody = msg.asJson.toString

        //        val reqBody = ModelRequest.newBuilder().setX("poop").setY(90124).setZ(8675308).build()

        val request =
          basicRequest
          //            .contentType("application/x-protobuf")
            .body(reqBody.getBytes)
            .response(asByteArray)
            .readTimeout(30 minutes)
            .post(uri"http://localhost:9900")

        println("request prepared")
        println(request)

        val backend = HttpURLConnectionBackend()

        println("backend prepared")

        val response = request.send(backend)

        println("request sent (sync), response received")

        println(response.body)
        println(response.headers)

        response.body match {
          case Left(failureMsg) =>
            fail(failureMsg)
          case Right(responseBody) =>
            val responseString = new String(responseBody)
            println(responseString)
        }
      }
    }
    "Server test case" should {
      "run to the end" in {

        // messages to send
        val episodeId = EpisodeId("floof")
        val msg1: PolicyClientRequest =
          StartEpisodeRequest(episode_id = Some(episodeId), training_enabled = true)
        val msg2: PolicyClientRequest = GetActionRequest(
          episode_id = EpisodeId("floof"),
          observation = Observation.MultiAgentObservation(
            Map(
              AgentId("group_1") -> List(List(0.0, 0.0, 10.0, 10.0, 0.10), List(-10.0, -10.0, 0.0, 0.0, 0.70)),
              AgentId("group_2") -> List(List(10.0, 10.0, 0.0, 00.0, 0.02), List(0.0, 0.0, -10.0, -10.0, 0.99))
            )
          )
//          action = Action.MultiAgentDiscreteAction(
//            Map(
//              AgentId("group_1") -> List(0, 7),
//              AgentId("group_2") -> List(8, 4)
//            )
//          )
        )

        // setup of HTTP calls
        val backend  = HttpURLConnectionBackend()
        val req1Body = msg1.asJson.toString
        val req1 = basicRequest
          .body(req1Body.getBytes)
          .readTimeout(5 minutes)
          .response(asByteArray)
          .post(uri"http://localhost:9900")
        val req2Body = msg2.asJson.toString
        val req2 = basicRequest
          .body(req2Body.getBytes)
          .readTimeout(5 minutes)
          .response(asByteArray)
          .post(uri"http://localhost:9900")

        // execute
        val effect = for {
          res1 <- Right("") //req1.send(backend).body
          res2 <- req2.send(backend).body
        } yield (new String(res1), new String(res2))

        effect match {
          case Left(error) => fail(error)
          case Right((startEpisodeResponse, getActionResponse)) =>
            decode[PolicyClientResponse](getActionResponse) match {
              case Left(error) => fail(error)
              case Right(logActionResponse) =>
                println(startEpisodeResponse)
                println(logActionResponse)
                succeed
            }
        }
      }
    }
  }
}

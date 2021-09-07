package edu.colorado.fitzgero.sotestbed.rllib

import scala.reflect.{classTag, ClassTag}

import cats.effect._

import io.circe.syntax._
import sttp.client3._
import sttp.client3.circe._
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest._
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse._
import io.circe
import sttp.client3.basicRequest

final class RLlibPolicyClient(host: String, port: Int) {

  var episodeId: Option[EpisodeId] = None

  def getEpisodeId: IO[EpisodeId] =
    this.episodeId match {
      case None =>
        IO.raiseError(new Error(s"get_action called before start_episode (no stored episode_id)"))
      case Some(episodeId) =>
        IO.pure(episodeId)
    }

  def startEpisode(episodeId: Option[EpisodeId] = None, trainingEnabled: Boolean = true): IO[StartEpisodeResponse] = {

    val msg: PolicyClientRequest = StartEpisodeRequest(episodeId, trainingEnabled)

    val result = for {
      res <- RLlibPolicyClient.send[StartEpisodeResponse](msg, host, port)
      _   <- IO.delay { this.episodeId = Some(res.episode_id) }
    } yield res

    result
  }

  def getAction(observation: Observation): IO[GetActionResponse] = {

    val result = for {
      episodeId <- getEpisodeId
      msg: PolicyClientRequest = GetActionRequest(episodeId, observation)
      res <- RLlibPolicyClient.send[GetActionResponse](msg, host, port)
    } yield res

    result
  }

  def logAction(observation: Observation, action: Action): IO[Unit] = {
    val result = for {
      episodeId <- getEpisodeId
      msg: PolicyClientRequest = LogActionRequest(episodeId, observation, action)
      _ <- RLlibPolicyClient.send[Any](msg, host, port)
    } yield ()

    result
  }

  def logReturns(reward: Reward, info: Map[String, String], done: Option[Map[AgentId, Boolean]]): IO[Unit] = {
    val result = for {
      episodeId <- getEpisodeId
      msg: PolicyClientRequest = LogReturnsRequest(episodeId, reward, info, done)
      _ <- RLlibPolicyClient.send[Any](msg, host, port)
    } yield ()

    result
  }

  def endEpisode(observation: Observation): IO[Unit] = {
    val result = for {
      episodeId <- getEpisodeId
      msg: PolicyClientRequest = EndEpisodeRequest(episodeId, observation)
      _ <- RLlibPolicyClient.send[Any](msg, host, port)
    } yield ()

    result
  }

}

object RLlibPolicyClient {

  /**
    * helper for use of STTP client
    * @param msg the message to send, which is a PolicyClientRequest
    * @param host the host string, e.g., http://localhost
    * @param port the port number to contact
    * @tparam T the type of response object
    * @return the effect of calling the server
    */
  def send[T: ClassTag](msg: PolicyClientRequest, host: String, port: Int): IO[T] = {
    AsyncHttpClientCatsBackend[IO]().flatMap { backend =>
      val reqBody = msg.asJson.toString
      val request =
        basicRequest
          .body(reqBody.getBytes)
          .response(asJson[PolicyClientResponse])
          .post(uri"$host:$port")

      for {
        sttpRes <- request.send(backend)
        res     <- RLlibPolicyClient.handleSttpResponse[T](sttpRes)
      } yield res
    }
  }

  def handleSttpResponse[T: ClassTag](
    res: Response[Either[ResponseException[String, circe.Error], PolicyClientResponse]]
  ): IO[T] = {
    res.body match {
      case Left(value) =>
        IO.raiseError(new Error(s"failure sending request from client", new Exception(value)))
      case Right(startEpisodeResponse) =>
        startEpisodeResponse match {
          case correctType: T =>
            IO.delay(correctType)
          case other =>
            IO.raiseError(
              new Error(
                s"received unexpected response $other which does not match expected type ${classTag[T]} "
              )
            )
        }
    }
  }

}

package edu.colorado.fitzgero.sotestbed.rllib

import scala.reflect.{classTag, ClassTag}

import cats.effect._
import cats.implicits._

import io.circe.syntax._
import sttp.client3._
import sttp.client3.circe._
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest._
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse._
import io.circe
import sttp.client3.basicRequest

object PolicyClientOps {

  /**
    * helper for use of STTP client to send many requests. this creates a single backend
    * object to service all of the calls.
    *
    * @param msg the message to send, which is a PolicyClientRequest
    * @param host the host string, e.g., http://localhost
    * @param port the port number to contact
    * @return the effect of calling the server
    */
  def send(
    msgs: List[PolicyClientRequest],
    host: String,
    port: Int,
    parallelism: Int
  ): IO[List[PolicyClientResponse]] = {
    AsyncHttpClientCatsBackend[IO]().flatMap { backend =>
      def _send(msg: PolicyClientRequest): IO[PolicyClientResponse] = {
        val reqBody = msg.asJson.toString
        val request =
          basicRequest
            .body(reqBody.getBytes)
            .response(asJson[PolicyClientResponse])
            .post(uri"$host:$port")
        for {
          sttpRes <- request.send(backend)
          annotated = sttpRes.body.left.map { t => new Error(s"error from rl server for msg $reqBody", t) }
          res <- IO.fromEither(annotated)
        } yield res
      }

      val batches = msgs.sliding(parallelism, parallelism).toList

      for {
        responses <- batches.traverse { _.parTraverse(_send) }
        _         <- backend.close()
      } yield responses.flatten
    }
  }

  /**
    * helper for use of STTP client to send one request. this creates a single backend
    * object to service all of the calls, which may lead to errors if too many concurrent
    * calls to send occur. in place of this, use the method above for sending more requests
    * at once.
    *
    * @param msg the message to send, which is a PolicyClientRequest
    * @param host the host string, e.g., http://localhost
    * @param port the port number to contact
    * @return the effect of calling the server
    */
  def send(msg: PolicyClientRequest, host: String, port: Int): IO[PolicyClientResponse] = {
    AsyncHttpClientCatsBackend[IO]().flatMap { backend =>
      val reqBody = msg.asJson.toString
      val request =
        basicRequest
          .body(reqBody.getBytes)
          .response(asJson[PolicyClientResponse])
          .post(uri"$host:$port")

      for {
        sttpRes <- request.send(backend)
        res     <- IO.fromEither(sttpRes.body)
        _       <- backend.close
      } yield res
    }
  }
}

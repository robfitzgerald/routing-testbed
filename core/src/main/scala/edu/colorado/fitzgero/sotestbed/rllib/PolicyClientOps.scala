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
    * @param parallelism number of parallel HTTP connections to use
    * @param failOnServerError if false, any server error respones will become "PolicyClientResponse.Empty" objects
    * @param logFn callback to invoke for logging purposes
    * @return the effect of calling the server
    */
  def send(
    msgs: List[PolicyClientRequest],
    host: String,
    port: Int,
    parallelism: Int,
    failOnServerError: Boolean,
    logFn: Option[(List[PolicyClientRequest], List[PolicyClientResponse]) => IO[Unit]]
  ): IO[List[PolicyClientResponse]] = {
    AsyncHttpClientCatsBackend[IO]().flatMap { backend =>
      val sendFn: PolicyClientRequest => IO[PolicyClientResponse] = _send(host, port, backend, failOnServerError)
      val batches                                                 = msgs.sliding(parallelism, parallelism).toList

      for {
        responses <- batches.traverse { _.parTraverse(sendFn) }.map { _.flatten }
        _         <- backend.close()
        _         <- logFn.map { f => f(msgs, responses) }.getOrElse(IO.unit)
      } yield responses
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
    * @param failOnServerError if false, any server error respones will become "PolicyClientResponse.Empty" objects
    * @param logFn callback to invoke for logging purposes
    * @return the effect of calling the server
    */
  def send(
    msg: PolicyClientRequest,
    host: String,
    port: Int,
    failOnServerError: Boolean,
    logFn: Option[(PolicyClientRequest, PolicyClientResponse) => IO[Unit]]
  ): IO[PolicyClientResponse] = {
    for {
      backend  <- AsyncHttpClientCatsBackend[IO]()
      response <- _send(host, port, backend, failOnServerError)(msg)
      _        <- logFn.map { f => f(msg, response) }.getOrElse(IO.unit)
    } yield response
    // AsyncHttpClientCatsBackend[IO]().flatMap { backend => _send(host, port, backend, failOnServerError)(msg) }
  }

  def _send(host: String, port: Int, backend: SttpBackend[IO, Any], failOnServerError: Boolean)(
    msg: PolicyClientRequest
  ): IO[PolicyClientResponse] = {
    val reqBody = msg.asJson.toString
    val request =
      basicRequest
        .body(reqBody.getBytes)
        .response(asJson[PolicyClientResponse])
        .post(uri"$host:$port")

    val result = request.send(backend).flatMap { sttpRes =>
      if (sttpRes.code.isServerError && failOnServerError) {
        IO.raiseError(new Error(s"received ${sttpRes.code} error ${sttpRes.statusText}"))
      } else if (sttpRes.code.isServerError || !failOnServerError) {
        IO.pure(PolicyClientResponse.Empty)
      } else {
        val annotated = sttpRes.body.left
          .map { t => new Error(s"error from rl server for msg $reqBody", t) }
        IO.fromEither(annotated)
      }
    }

    result
  }
}

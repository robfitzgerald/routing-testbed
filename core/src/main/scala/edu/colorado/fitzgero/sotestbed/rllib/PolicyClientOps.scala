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

object PolicyClientOps {

  /**
    * helper for use of STTP client
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
      } yield res
    }
  }
}

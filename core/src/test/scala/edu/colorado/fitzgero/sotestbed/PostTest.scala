package edu.colorado.fitzgero.sotestbed

import sttp.client3._
//import edu.colorado.fitzgero.sotestbed.proto.v1._
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest._
import io.circe.syntax._

class PostTest extends SoTestBedBaseTest {
  "POST" when {
    "called" should {
      "result in response" in {

        val msg: PolicyClientRequest = StartEpisodeRequest(episode_id = None, training_enabled = true)
        val reqBody                  = msg.asJson.toString

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
  }
}

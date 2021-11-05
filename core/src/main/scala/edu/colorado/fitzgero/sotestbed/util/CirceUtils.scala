package edu.colorado.fitzgero.sotestbed.util

import io.circe.{Decoder, Encoder}
import cats.implicits._

object CirceUtils {

  /**
    * provides an encoder for a Map[K1, V1] assuming we have a Map[K2, V2] encoder
    *
    * @param kFn
    * @param vFn
    * @param me
    * @tparam K1
    * @tparam V1
    * @tparam K2
    * @tparam V2
    * @return
    */
  def mapEncoder[K1, V1, K2, V2](
    kFn: K1 => K2,
    vFn: V1 => V2
  )(implicit me: Encoder[Map[K2, V2]]): Encoder[Map[K1, V1]] =
    Encoder[Map[K2, V2]].contramap { _.map { case (k, v) => kFn(k) -> vFn(v) } }

  def mapDecoder[K1, V1, K2, V2](kFn: K2 => Either[String, K1], vFn: V2 => Either[String, V1])(
    implicit md: Decoder[Map[K2, V2]]
  ): Decoder[Map[K1, V1]] =
    Decoder[Map[K2, V2]].emap { m =>
      m.toList
        .traverse {
          case (k, v) =>
            for {
              kUpdate <- kFn(k)
              vUpdate <- vFn(v)
            } yield (kUpdate, vUpdate)
        }
        .map {
          _.toMap
        }
    }
}

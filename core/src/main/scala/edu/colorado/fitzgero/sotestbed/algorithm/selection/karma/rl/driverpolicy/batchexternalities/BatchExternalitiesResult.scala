package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.batchexternalities

import io.circe.generic.semiauto._
import io.circe.Encoder

final case class BatchExternalitiesResult(
  value: Double,
  lowerBoundValue: Double,
  upperBoundValue: Double
)

object BatchExternalitiesResult {
  implicit val enc: Encoder[BatchExternalitiesResult] = deriveEncoder
}

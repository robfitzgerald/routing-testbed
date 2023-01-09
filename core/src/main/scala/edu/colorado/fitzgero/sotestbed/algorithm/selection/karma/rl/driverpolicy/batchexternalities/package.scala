package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

import cats.effect.IO

package object batchexternalities {

  // (UpperBounds, LowerBounds) => Result
  type BatchExternalitiesMetric = (List[Double], List[Double]) => IO[BatchExternalitiesResult]

}

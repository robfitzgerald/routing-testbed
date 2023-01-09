package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.batchexternalities

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness.JainFairnessMath
import cats.effect.IO

object BatchExternalitiesMetric {

  val jainDiff: BatchExternalitiesMetric = (best: List[Double], worst: List[Double]) => {
    val diffResult = for {
      ub <- JainFairnessMath.fairness(best)
      lb <- JainFairnessMath.fairness(worst)
    } yield BatchExternalitiesResult(ub - lb, lb, ub)
    IO.fromOption(diffResult)(new Error(s"unable to compute fairness of batch"))
  }

  val studentsTTest: BatchExternalitiesMetric = (best: List[Double], worst: List[Double]) => {
    ???
  }

}

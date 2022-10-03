package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness

object AuctionDelayAllocationMetric {

  /**
    * computes the metric value based on the changes in travel time estimates
    * due to auction outcomes. the piecewise function looks at the absolute
    * value of delays proportional to the final travel time and bounds the
    * range to [0, 1].
    *
    * delay and improvement values are accumulated (summed) together and
    * divided by the final travel time only when the accumulated value is
    * negative (delay) and smaller than the magnitude of the final travel time.
    *
    * @param requestTimeEstimates
    * @param afterSelectionEstimates
    * @param finalTravelTime
    * @return
    */
  def computeMetricValue(
    requestTimeEstimates: List[Double],
    afterSelectionEstimates: List[Double],
    finalTravelTime: Double
  ): Either[Error, Double] = {
    val lComp = requestTimeEstimates.lengthCompare(afterSelectionEstimates)
    if (lComp != 0) {
      Left(new Error(s"result of requestTimeEstimates.lengthCompare(afterSelectionEstimates) was $lComp"))
    } else {
      val tuples   = requestTimeEstimates.zip(afterSelectionEstimates)
      val accDelta = tuples.foldLeft(0.0) { case (acc, (before, after)) => acc + (before - after) }
      val metricValue =
        if (accDelta < 0.0 && math.abs(accDelta) > finalTravelTime) 0.0
        else if (accDelta < 0.0) 1 - (math.abs(accDelta) / finalTravelTime)
        else 1.0
      Right(metricValue)
    }
  }
}

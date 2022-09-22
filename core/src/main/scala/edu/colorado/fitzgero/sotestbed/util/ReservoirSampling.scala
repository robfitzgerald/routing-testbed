package edu.colorado.fitzgero.sotestbed.util

import scala.util.Random
import scala.annotation.tailrec
import com.typesafe.scalalogging.LazyLogging

object ReservoirSampling extends LazyLogging {

  /**
    * Use Efraimidis and Spirakis' "Algorithm A-ExpJ" for weighted reservoir sampling
    * without replacement.
    *
    * [[see https://en.wikipedia.org/wiki/Reservoir_sampling#Algorithm_A-ExpJ]]
    *
    * @param rng random generator
    * @param populationWithWeights items with their non-negative numeric weight values
    * @param k number of items to sample
    * @return the population partitioned into a sample population and the remaining un-sampled population
    */
  def aExpJ[T](
    rng: Random,
    populationWithWeights: List[(T, Double)],
    k: Int
  ): (List[(T, Double)], List[(T, Double)]) = {
    if (populationWithWeights.lengthCompare(k) <= 0) (populationWithWeights, List.empty)
    else {
      // create a min priority queue over the weighted population
      implicit val ord: Ordering[(T, Double)] = Ordering.by { case (_, w) => -w }
      val reservoir                           = scala.collection.mutable.PriorityQueue[(T, Double)]()

      // compute a key value for entry into the reservoir from an item weight
      def computeKey(w: Double, t: Option[Double] = None): Double = {
        val randomValue = t match {
          case None => rng.nextDouble // random(0, 1) case
          case Some(value) =>
            val tValueTruncated = math.max(0.0, math.min(1.0, value))
            if (tValueTruncated == 1.0) 1.0
            else rng.between(tValueTruncated, 1.0) // random(t, 1) case
        }
        val pow = if (w == 0.0) 1.0 else 1.0 / w
        math.pow(randomValue, pow)
      }

      // (re-)compute X via the min key value
      def recomputeX(): Double = {
        val (_, minKey) = reservoir.head
        val x           = math.log(rng.nextDouble) / math.log(math.max(1.0, minKey))
        x
      }

      // fill the reservoir with an initial solution
      val initialSolutionEntries = populationWithWeights.take(k).map { case (t, w) => (t, computeKey(w)) }
      reservoir.addAll(initialSolutionEntries)

      // step through the remaining population and make updates to the solution queue, keeping
      // track of all discarded values in the "notSampled" collection
      @tailrec
      def _solve(
        x: Double,
        remaining: List[(T, Double)],
        notSampled: List[(T, Double)] = List.empty
      ): List[(T, Double)] = {
        remaining match {
          case Nil => notSampled
          case (nextItem, nextWeight) :: tail =>
            val nextX = x - nextWeight
            if (nextX > 0) _solve(nextX, tail, (nextItem, nextWeight) +: notSampled)
            else {
              // replace the min-weight value in the reservoir with this entry and reset X
              val (_, minKey) = reservoir.head
              val t           = math.pow(minKey, nextWeight)
              val nextKey     = computeKey(nextWeight, Some(t))
              val removedItem = reservoir.dequeue()
              reservoir.addOne((nextItem, nextWeight))
              val resetX = recomputeX()
              _solve(resetX, tail, removedItem +: notSampled)
            }
        }
      }

      val notSampled = _solve(recomputeX(), populationWithWeights.drop(k))
      (reservoir.toList, notSampled)
    }

  }

}

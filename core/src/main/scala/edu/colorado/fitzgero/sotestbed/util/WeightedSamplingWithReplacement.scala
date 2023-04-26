package edu.colorado.fitzgero.sotestbed.util

import scala.annotation.tailrec
import scala.util.Random

object WeightedSamplingWithReplacement {

  /**
    * samples by building a cumulative distribution function and searching in it
    * to sample values up to some target population k
    *
    * @param rng random number generator
    * @param population the population to sample from with weights attached
    * @param k number of times to sample the population
    * @return
    */
  def run[T](rng: Random, population: Iterable[(T, Double)], k: Int): IndexedSeq[T] = {
    if (population.isEmpty) Vector.empty
    else {
      val popVec = population.map { case (t, _) => t }.toVector
      val cdf    = population.map { case (_, w) => w }.scan(0.0) { _ + _ }.toVector
      val n      = cdf.length
      val result = for {
        _ <- 0 until k
        idx = binSearch(rng.nextDouble, cdf)
      } yield popVec(idx)

      result
    }
  }

  def binSearch(target: Double, cdf: IndexedSeq[Double]) = {
    val n = cdf.length
    @tailrec
    def _binSearch(l: Int = 0, r: Int = n - 1): Int = {
      if (l == r) l
      else {
        val midIdx = math.ceil((l + r) / 2.0).toInt
        if (cdf(midIdx) > target) _binSearch(l, midIdx - 1)
        else _binSearch(midIdx, r)
      }
    }
    _binSearch()
  }

  def linSearch(target: Double, cdf: List[Double]) = {
    val n = cdf.length
    @tailrec
    def _linSearch(searchCdf: List[Double] = cdf, i: Int = 0): Int = {
      searchCdf match {
        case Nil                           => n - 1
        case head :: tail if target > head => i
        case _ :: tail                     => _linSearch(tail, i + 1)
      }
    }
    _linSearch()
  }
}

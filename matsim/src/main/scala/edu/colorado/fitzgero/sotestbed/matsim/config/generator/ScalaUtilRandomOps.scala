package edu.colorado.fitzgero.sotestbed.matsim.config.generator

import scala.util.Random

object ScalaUtilRandomOps {

  implicit class RandomOpsInstance(random: Random) {

    def uniformInRange(low: Int, high: Int): Int = {
      val upperBound = high - low
      val result     = random.nextInt(upperBound) + low
      result
    }

    def uniformInRange(low: Double, high: Double): Double = {
      val upperBound = high - low
      val result     = (random.nextDouble * upperBound) + low
      result
    }

    def gaussianInRange(low: Double, mean: Double, high: Double, scale: Double = 0.16): Double = {
      // scala gaussian uses std deviation of 1.0 whos 95% covers +- 2.5 -> scale down by 0.4
      // try again, how about 1/3.5?
      val result        = (random.nextGaussian * scale) + mean
      val resultBounded = math.max(low, math.min(high, result))
      resultBounded
    }

    def gaussianInIntegerRange(low: Int, mean: Int, high: Int, scale: Option[Int] = None): Int = {
      val intScale = scale.getOrElse((high - low) / 8)
      gaussianInRange(low.toDouble, mean.toDouble, high.toDouble, intScale.toDouble).toInt
    }
  }
}

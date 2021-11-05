package edu.colorado.fitzgero.sotestbed.algorithm.selection.rl

object RLUtils {

  /**
    * apply the given percentage to the given max value, rounding
    * to the nearest integer.
    * @param pct percentage to apply
    * @param max maximum integer in range
    * @return the closest integer to pct * max
    */
  def mapPercentageToIntegerRange(pct: Double, max: Int): Int = {
    math.round(max.toDouble * pct).toInt
  }
}

package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness

/**
  * @deprecated this implies configurable distribution transforms. i've moved
  * away from this to instead specifying unique AllocationMetric instances with
  * each their own interpretation of how we handle the allocation distribution.
  */
sealed trait AllocationTransform

object AllocationTransform {

  case object NoTransform extends AllocationTransform

  /**
    * use the default builder (See below)
    */
  case object Default extends AllocationTransform

  /**
    * use the default builder but pass in a different limit value.
    * that value is usually 0 (for "zero seconds") but here a different
    * limit could be set, such as 300 (for "five minutes")
    *
    * @param limit the limit to use
    */
  case class DefaultWithLimit(limit: Double) extends AllocationTransform

  /**
    * just shift the data by some magnitude in either pos or neg direction
    *
    * @param amount the direction (neg, pos) and magnitude of the shift
    */
  case class Shift(amount: Double) extends AllocationTransform

  /**
    * shifts the values into the range [0, |lower|]
    */
  case object ShiftPositive extends AllocationTransform

  /**
    * normalizes the values into the range [0, 1]
    */
  case object Normalize extends AllocationTransform

  /**
    * truncates all values below some limit
    */
  case class TruncateLessThanOrEqual(limit: Double) extends AllocationTransform

  /**
    * combination (tree) of transforms
    *
    * @param head first transform(s) to apply
    * @param tail rest to apply
    */
  case class Combined(head: AllocationTransform, tail: AllocationTransform) extends AllocationTransform

  /**
    * original default transform
    * 1. removes all "travel time improvement" above the $limit,
    * 2. shifts those values to positive numbers
    *
    * @param limit ignore travel time improvement above this value
    */
  def v1Transform(limit: Double = 0.0): AllocationTransform =
    Combined(
      TruncateLessThanOrEqual(0.0),
      ShiftPositive
    )

  val DefaultTransform = NoTransform

  implicit class AllocationTransformOps(t: AllocationTransform) {

    /**
      * applies a transform to a collection of values assumed to be in the range
      * (-inf, 0]. this prepares the values for a learning task.
      *
      * @param xs the values to transform
      * @return the values transformed via the chosen [[AllocationTransform]] function
      */
    def applyTransform(xs: List[Double]): List[Double] =
      if (xs.isEmpty) xs
      else
        t match {
          case NoTransform => xs
          case Default =>
            DefaultTransform.applyTransform(xs)
          case DefaultWithLimit(limit) =>
            v1Transform(limit).applyTransform(xs)
          case Shift(mag) =>
            xs.map { _ + mag }
          case ShiftPositive =>
            val lower = xs.min
            if (lower >= 0.0) xs
            else {
              val shiftValue = math.abs(lower)
              val shifted    = xs.map { _ + shiftValue }
              shifted
            }
          case Normalize =>
            val (lower, upper) = (xs.min, xs.max)
            val denom          = upper - lower
            if (denom == 0.0) xs
            else xs.map { x => (x - lower) / (upper - lower) }
          case TruncateLessThanOrEqual(limit) =>
            xs.map {
              case x if x > limit => limit
              case x              => x
            }
          case Combined(head, tail) =>
            val headResult = head.applyTransform(xs)
            val tailResult = tail.applyTransform(headResult)
            tailResult
        }

  }

}

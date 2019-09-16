package edu.colorado.fitzgero.sotestbed.model.numeric

import scala.Numeric.Implicits._

final class SimTime(val value: Long) extends AnyVal

object SimTime {
  val Zero: SimTime = new SimTime(0)
  def apply[T: Numeric](time: T): SimTime = new SimTime(time.toLong)
}
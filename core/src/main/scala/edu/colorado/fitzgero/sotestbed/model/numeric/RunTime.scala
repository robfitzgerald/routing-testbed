package edu.colorado.fitzgero.sotestbed.model.numeric

final class RunTime(val value: Double) extends AnyVal {
  def +(that: RunTime): RunTime = RunTime(this.value + that.value)
  def -(that: RunTime): RunTime = RunTime(this.value - that.value)
  def <(that: Double): Boolean  = this.value < that
  def <=(that: Double): Boolean = this.value < that
  def >(that: Double): Boolean  = this.value > that
  def >=(that: Double): Boolean = this.value >= that
  override def toString: String = this.value.toString

  def asMinutes: String = {
    val minutes = math.floor(value / 1000.0 / 60.0).toInt
    val seconds = (value / 1000.0).toInt % 60
    val millis  = value.toInt % 1000

    val minStr = if (minutes < 10) s"0$minutes" else s"$minutes"
    val secStr = if (seconds < 10) s"0$seconds" else s"$seconds"
    val msStr = {
      if (millis < 10) s"000$millis"
      else if (millis < 100) s"00$millis"
      else if (millis < 1000) s"0$millis"
      else s"$millis"
    }
    s"$minStr:$secStr.$msStr"
  }
}

object RunTime {
  def apply(value: Double): RunTime = new RunTime(value)
  val Zero: RunTime                 = new RunTime(0.0)
  val Now: RunTime                  = RunTime(System.currentTimeMillis)
}

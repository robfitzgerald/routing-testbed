package edu.colorado.fitzgero.sotestbed.matsim.simulator

import org.matsim.api.core.v01.population.Leg

final case class DepartureTime(value: Int) extends AnyVal {
  override def toString: String = {
    def padLeft(n: String): String = if (n.length == 1) s"0$n" else n
    val hour: String =
      padLeft((this.value / 3600).toString)
    val min: String = padLeft(((this.value % 3600) / 60).toString)
    val sec: String = padLeft((this.value  % 60).toString)
    s"$hour:$min:$sec"
  }
}

object DepartureTime {

  def getLegDepartureTime(leg: Leg): Option[DepartureTime] = {
    if (leg.getDepartureTime.isDefined) Some(DepartureTime(leg.getDepartureTime.seconds.toInt)) else None
  }
}

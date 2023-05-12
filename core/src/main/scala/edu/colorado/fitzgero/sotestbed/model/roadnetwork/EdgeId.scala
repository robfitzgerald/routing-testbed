package edu.colorado.fitzgero.sotestbed.model.roadnetwork

final class EdgeId(val value: String) extends AnyVal {
  override def toString: String = value
}

object EdgeId {
  def apply(s: String): EdgeId = new EdgeId(s)
}

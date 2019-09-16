package edu.colorado.fitzgero.sotestbed.model.roadnetwork

final class EdgeId (val value: String) extends AnyVal

object EdgeId {
  def apply(s: String): EdgeId = new EdgeId(s)
}

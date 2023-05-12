package edu.colorado.fitzgero.sotestbed.model.roadnetwork

final class VertexId(val value: String) extends AnyVal {
  override def toString = value
}

object VertexId {
  def apply(s: String): VertexId = new VertexId(s)
}

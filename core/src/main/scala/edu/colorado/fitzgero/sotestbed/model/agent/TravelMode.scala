package edu.colorado.fitzgero.sotestbed.model.agent

sealed trait TravelMode
object TravelMode {
  final case object Car extends TravelMode {
    override def toString: String = "car"
  }

  def fromString(string: String): Option[TravelMode] =
    string.toLowerCase match {
      case "car" => Some{Car}
      case _ => None
    }
}
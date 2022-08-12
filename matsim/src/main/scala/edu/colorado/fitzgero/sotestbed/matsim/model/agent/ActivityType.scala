package edu.colorado.fitzgero.sotestbed.matsim.model.agent

sealed trait ActivityType

object ActivityType {

  final case object Home extends ActivityType {
    override def toString: String = "home"
  }

  final case object Work extends ActivityType {
    override def toString: String = "work"
  }
}

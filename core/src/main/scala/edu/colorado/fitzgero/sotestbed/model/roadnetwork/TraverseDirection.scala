package edu.colorado.fitzgero.sotestbed.model.roadnetwork

sealed trait TraverseDirection

object TraverseDirection {
  final case object Forward extends TraverseDirection
  final case object Reverse extends TraverseDirection
}

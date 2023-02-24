package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Bid
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path

sealed trait DriverPolicyFilter

object DriverPolicyFilter {

  final case object NoFilter extends DriverPolicyFilter

  final case object RemoveSoAssignments extends DriverPolicyFilter

  implicit class NPFExtension(npf: DriverPolicyFilter) {

    def filter(selections: List[(Bid, Int, Path)]): List[(Bid, Int, Path)] = npf match {
      case NoFilter            => selections
      case RemoveSoAssignments => selections.filter { case (_, idx, _) => idx == 0 }
    }

  }

}

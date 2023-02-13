package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.networkpolicy

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Bid
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path

sealed trait NetworkPolicyFilter

object NetworkPolicyFilter {

  final case object NoFilter extends NetworkPolicyFilter

  final case object RemoveSoAssignments extends NetworkPolicyFilter

  implicit class NPFExtension(npf: NetworkPolicyFilter) {

    def filter(selections: List[(Bid, Int, Path)]): List[(Bid, Int, Path)] = npf match {
      case NoFilter            => selections
      case RemoveSoAssignments => selections.filter { case (_, idx, _) => idx == 0 }
    }

  }

}

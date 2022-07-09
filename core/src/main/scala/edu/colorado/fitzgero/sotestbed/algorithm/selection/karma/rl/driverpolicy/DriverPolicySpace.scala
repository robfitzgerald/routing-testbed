package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma

sealed trait DriverPolicySpace

object DriverPolicySpace {

  case object UrgencyAndBalance extends DriverPolicySpace

  implicit class DPSExtensions(dps: DriverPolicySpace) {

    def encodeObservation(
      request: Request,
      balance: Karma,
      history: ActiveAgentHistory.AgentHistory,
      proposedPaths: List[Path]
    ): List[Double] = {
      dps match {
        case UrgencyAndBalance =>
          // observe the agent urgency and the agent's balance
          ???
      }
    }
  }
}

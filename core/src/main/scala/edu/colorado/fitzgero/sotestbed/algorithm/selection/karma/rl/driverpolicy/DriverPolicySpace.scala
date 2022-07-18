package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import cats.effect.IO
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost

sealed trait DriverPolicySpace

object DriverPolicySpace {

  /**
    * observes the agent's current karma balance, along with the difference
    * in estimated travel time between the agent's first route plan and their
    * current route plan
    */
  case object BalanceAndUrgency extends DriverPolicySpace

  /**
    * observes the agent's current karma balance, the urgency as defined
    * above, along with the maximum possible travel time difference due
    * to taking one of the proposed routes.
    */
  case object BalanceUrgencyAndWorstAlternative extends DriverPolicySpace

  implicit class DPSExtensions(dps: DriverPolicySpace) {

    /**
      * encodes an observation based on the current network state for
      * some request along with information based on its history and
      * the proposed set of routes for this agent.
      *
      * @param rn the current road network state
      * @param cf a cost function
      * @param request the current agent request
      * @param balance the current karma balance for this agent
      * @param history the route plan history for this agent
      * @param proposedPaths the set of paths that the agent could be assigned a new route from
      * @return the effect of collecting a list of observation values
      */
    def encodeObservation(
      rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
      cf: EdgeBPR => Cost
    )(
      request: Request,
      balance: Karma,
      history: ActiveAgentHistory.AgentHistory,
      proposedPaths: List[Path]
    ): IO[List[Double]] = {
      dps match {
        case BalanceAndUrgency =>
          // observe the agent urgency and the agent's balance
          for {
            urgency <- ObservationOps.travelTimeDiffFromInitialTrip(rn, cf, history)
          } yield List(urgency, balance.value)

        case BalanceUrgencyAndWorstAlternative =>
          for {
            urgency   <- ObservationOps.travelTimeDiffFromInitialTrip(rn, cf, history)
            altsDiffs <- ObservationOps.travelTimeDiffFromAlternatives(rn, cf, history, proposedPaths)
          } yield List(urgency, balance.value, altsDiffs.max)
      }
    }
  }
}

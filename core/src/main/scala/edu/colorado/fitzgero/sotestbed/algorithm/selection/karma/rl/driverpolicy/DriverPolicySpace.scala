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
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

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
      request: Request,
      balance: Karma,
      history: ActiveAgentHistory.AgentHistory,
      proposedPaths: List[Path]
    ): IO[List[Double]] = {
      dps match {
        case BalanceAndUrgency =>
          // observe the agent urgency and the agent's balance
          for {
            urgency <- ObservationOps.travelTimeDiffFromInitialTrip(history)
          } yield List(balance.value, urgency)

        case BalanceUrgencyAndWorstAlternative =>
          for {
            urgency   <- ObservationOps.travelTimeDiffFromInitialTrip(history)
            altsDiffs <- ObservationOps.travelTimeDiffFromAlternatives(history, proposedPaths)
          } yield List(balance.value, urgency, altsDiffs.max)
      }
    }

    def encodeFinalObservation(
      originalTravelTimeEstimate: SimTime,
      finalTravelTime: SimTime,
      finalBankBalance: Karma
    ): IO[List[Double]] = dps match {
      case BalanceAndUrgency =>
        val urgency = (finalTravelTime - originalTravelTimeEstimate).value.toDouble
        val obs     = List(finalBankBalance.value.toDouble, urgency)
        IO.pure(obs)
      case BalanceUrgencyAndWorstAlternative =>
        val urgency          = (finalTravelTime - originalTravelTimeEstimate).value.toDouble
        val worstAlternative = 0.0
        val obs              = List(finalBankBalance.value.toDouble, urgency, worstAlternative)
        IO.pure(obs)
    }
  }
}

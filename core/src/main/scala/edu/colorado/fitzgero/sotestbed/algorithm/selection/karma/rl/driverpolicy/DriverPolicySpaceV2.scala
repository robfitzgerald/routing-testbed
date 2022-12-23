package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.algorithm.batching._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import cats.effect.IO
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.config.DriverPolicyConfig
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig._
import edu.colorado.fitzgero.sotestbed.model.numeric.Meters
import java.sql.Driver

sealed trait DriverPolicySpaceV2

object DriverPolicySpaceV2 {

  final case class Combined(features: List[DriverPolicySpaceV2]) extends DriverPolicySpaceV2

  /**
    * diff between the fairness of the most unfair result and the most fair result.
    * when there is no difference, the value is 0 (best), and when the value is
    * high (approaching 1), it is a bad time for all.
    *
    * @param invertResult if true, invert the result so that 1 is best instead
    */
  final case class BatchUnfairnessExternalities(invertResult: Boolean) extends DriverPolicySpaceV2

  /**
    * ff/t for travel time estimate _t_ and free flow travel time _ff_, both
    * for the current trip plan. _t_ assumed to be greater than or equal to _ff_
    * but we still truncate the result to [0, 1].
    * if the result is 0, it means there has been little delay over the free
    * flow travel time. if it's nearer to 1, then there has been lots of delay.
    *
    * @param invertResult if true, invert the result so that 1 is best instead
    */
  final case class FreeFlowOverTravelTimePercent(invertResult: Boolean) extends DriverPolicySpaceV2

  implicit class DPSV2Extensions(dps: DriverPolicySpaceV2) {

    // bring in all helper functions used for computing the observations
    import DriverPolicySpaceV2Ops._

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
      req: Request,
      bal: Karma,
      rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
      hists: ActiveAgentHistory,
      paths: List[Path],
      alts: Map[Request, List[Path]],
      sig: NetworkPolicySignal
    ): IO[List[Double]] = dps match {

      case Combined(features) =>
        features.flatTraverse { _.encodeObservation(req, bal, rn, hists, paths, alts, sig) }

      case BatchUnfairnessExternalities(invertResult) =>
        val fn = BatchFairnessExternalities.jainDiff
        for {
          extResult <- BatchFairnessExternalities.calculate(rn, alts, sig, hists, fn)
          obsTrunc = extResult.differenceTruncated
          obs      = if (invertResult) 1.0 - obsTrunc else obsTrunc
        } yield List(obs)

      case FreeFlowOverTravelTimePercent(invertResult) =>
        for {
          hist       <- IO.fromEither(hists.getAgentHistoryOrError(req.agent))
          currentReq <- IO.fromEither(hist.currentRequest)
          obsResult  <- freeFlowOverTravelTimePercent(rn, hist, currentReq.remainingRoute)
          obs = if (invertResult) 1.0 - obsResult else obsResult
        } yield List(obs)
    }
  }
}

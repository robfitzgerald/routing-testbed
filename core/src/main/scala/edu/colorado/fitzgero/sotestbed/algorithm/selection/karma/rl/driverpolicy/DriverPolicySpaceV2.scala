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

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.batchexternalities._

/**
  * an ADT representing the different observation features for a driver agent
  * which may contribute to choosing a Bid.
  *
  * - [[Combined]]
  *   - allows specifying more than one feature
  * - [[ExperiencedDistancePercent]]
  *   - from no distance (0) to reaching destination (1)
  *   - "about how much longer do we have to make bids"
  * - [[FreeFlowOverTravelTimePercent]]
  *   - from same (1) to travel time >> free flow (~0)
  *   - "how much delay have we accrued"
  * - [[RiskOffset]]
  *   - from no risk (min) to high risk (~max), default [0,1]
  *   - "what's the risk *to us* for losing this auction"
  * - [[BatchRisk]]
  *   - from no risk (min) to high risk (~max), default [0,1]
  *   - "what's the risk *to the batch* for choosing poorly"
  */
sealed trait DriverPolicySpaceV2

object DriverPolicySpaceV2 {

  final case class Combined(features: List[DriverPolicySpaceV2]) extends DriverPolicySpaceV2

  /**
    * the distance experienced divided by the expected overall distance
    *
    * @param invertResult if true, invert the result so that 1 is best instead
    *                     ("percent distance remaining")
    */
  final case class ExperiencedDistancePercent(invertResult: Boolean = false) extends DriverPolicySpaceV2

  /**
    * ff/t for travel time estimate _t_ and free flow travel time _ff_, both
    * for the current trip plan. _t_ assumed to be greater than or equal to _ff_
    * but we still limit the result to [0, 1] to omit edge cases.
    *
    * if the result is 1, it means there has been little delay over the free
    * flow travel time. if it's nearer to 0, then there has been lots of delay.
    *
    * @param invertResult if true, invert the result so that 0 is best instead
    */
  final case class FreeFlowOverTravelTimePercent(invertResult: Boolean = false) extends DriverPolicySpaceV2

  /**
    * the risk to this driver as the offset of the UO vs SO trip assignment.
    * these values are divided by the freeflow travel time to scale them so
    * that the risk is "lower" as both outcomes are closer to free flow speed.
    *
    * @param invertResult if true, invert the result so that 1 is best instead
    * @param minOffset in the case that SO < UO, offset can be negative. this sets
    *                  a min value to limit by, default 0.0
    * @param maxOffset offset is unbounded; this max value is used to limit results.
    *                  by default the value is 1.0
    */
  final case class RiskOffset(
    invertResult: Boolean = false,
    minOffset: Double = 0.0,
    maxOffset: Double = 1.0
  ) extends DriverPolicySpaceV2

  /**
    * this is a batch-level observation that is the same for each member of the batch.
    *
    * diff between the fairness of the most unfair result and the most fair result.
    * when there is no difference, the value is 0 (best), and when the value is
    * high (approaching 1), it is a bad time for all.
    *
    * @param invertResult if true, invert the result so that 1 is best instead
    * @param minRisk limit risk to at least this value, default 0.0
    * @param maxRisk limit risk to as most this value, default 0.0
    */
  final case class BatchRisk(
    invertResult: Boolean = false,
    minRisk: Double = 0.0,
    maxRisk: Double = 1.0
  ) extends DriverPolicySpaceV2

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

      case ExperiencedDistancePercent(invertResult) =>
        for {
          hist <- IO.fromEither(hists.getAgentHistoryOrError(req.agent))
          req  <- IO.fromEither(hist.currentRequest)
          dist = req.experiencedDistance.value / req.overallDistance.value
          obs  = if (invertResult) 1.0 - dist else dist
        } yield List(obs)

      case FreeFlowOverTravelTimePercent(invertResult) =>
        for {
          hist       <- IO.fromEither(hists.getAgentHistoryOrError(req.agent))
          currentReq <- IO.fromEither(hist.currentRequest)
          obsResult  <- freeFlowOverTravelTimePercent(rn, hist)
          obs = if (invertResult) 1.0 - obsResult else obsResult
        } yield List(obs)

      case RiskOffset(invertResult, minOffset, maxOffset) =>
        for {
          hist   <- IO.fromEither(hists.getAgentHistoryOrError(req.agent))
          uoSpur <- getUoPathAlternative(req, alts)
          soSpur <- getSoPathAlternative(req, alts)
          uoTime <- pathAlternativeTravelTimeEstimate(rn, hist, uoSpur)
          soTime <- pathAlternativeTravelTimeEstimate(rn, hist, soSpur)
          offset        = if (uoTime == 0.0) 0.0 else (soTime - uoTime) / uoTime
          offsetLimited = math.max(minOffset, math.min(maxOffset, offset))
          obs           = if (invertResult) 1.0 - offsetLimited else offsetLimited
        } yield List(obs)

      case BatchRisk(invertResult, minRisk, maxRisk) =>
        // could be a config parameter if we explore alternatives like t-test
        val fn = BatchExternalitiesMetric.jainDiff
        for {
          extResult <- BatchFairnessExternalities.calculate(rn, alts, sig, hists, fn)
          obsLimited = math.max(minRisk, math.min(maxRisk, extResult.value))
          obs        = if (invertResult) 1.0 - obsLimited else obsLimited
        } yield List(obs)

    }

    def encodeFinalObservation(
      originalTravelTimeEstimate: SimTime,
      finalTravelTime: SimTime,
      freeFlowTravelTime: SimTime,
      finalDistance: Meters,
      finalBankBalance: Karma,
      finalReplannings: Int,
      finalUoRoutesAssigned: Int,
      networkPolicyConfig: NetworkPolicyConfig
    ): IO[List[Double]] = dps match {

      case Combined(ls) =>
        ls.flatTraverse(
          _.encodeFinalObservation(
            originalTravelTimeEstimate,
            finalTravelTime,
            freeFlowTravelTime,
            finalDistance,
            finalBankBalance,
            finalReplannings,
            finalUoRoutesAssigned,
            networkPolicyConfig
          )
        )

      case ExperiencedDistancePercent(invertResult) =>
        val obs = if (invertResult) 0.0 else 1.0
        IO.pure(List(obs))

      case FreeFlowOverTravelTimePercent(invertResult) =>
        val pct      = freeFlowTravelTime.value / finalTravelTime.value
        val pctLimit = math.max(0.0, math.min(1.0, pct))
        val obs      = if (invertResult) 1.0 - pctLimit else pctLimit
        IO.pure(List(obs))

      case RiskOffset(invertResult, minOffset, maxOffset) =>
        val obs = if (invertResult) 1.0 else 0.0
        IO.pure(List(obs))

      case BatchRisk(invertResult, minRisk, maxRisk) =>
        val obs = if (invertResult) 1.0 else 0.0
        IO.pure(List(obs))
    }
  }
}

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
import com.typesafe.scalalogging.LazyLogging

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
  * - [[AssignmentExternalities]]
  *   - from worse (min) to best (max) fairness
  *   - "marginal fairness when this agent is re-routed"
  */
sealed trait DriverPolicySpaceV2

object DriverPolicySpaceV2 extends LazyLogging {

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
    * @param maxPercent limit values to maxPercent, default 100% (1.0)
    */
  final case class FreeFlowOverTravelTimePercent(invertResult: Boolean = false, maxPercent: Double = 1.0)
      extends DriverPolicySpaceV2

  /**
    * the risk to this driver as the offset of the UO vs SO trip assignment travel times
    * computed as (so - uo) / uo.
    *
    * NO LONGER TRUE: "these values are divided by the freeflow travel time to scale them so
    * that the risk is "lower" as both outcomes are closer to free flow speed."
    *
    * @param invertResult if true, invert the result so that 1 is best instead
    * @param minOffset in the case that SO < UO, offset can be negative. this sets
    *                  a min value to limit by, default 0.0
    * @param maxOffset offset is unbounded; this max value is used to limit results.
    *                  by default the value is 1.0
    */
  final case class RiskOffset(invertResult: Boolean = false, minOffset: Double = 0.0, maxOffset: Double = 1.0)
      extends DriverPolicySpaceV2

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
  final case class BatchRisk(invertResult: Boolean = false, minRisk: Double = -1.0, maxRisk: Double = 1.0)
      extends DriverPolicySpaceV2

  /**
    * this is a batch-level observation that is the different for each member of the batch.
    *
    * diff between the fairness of the current route plans for this batch, comparing with
    * the fairness of assigning a new path for just one agent ($this agent).
    * when the value is positive, assigning for this agent marginally improves fairness;
    * when negative, it is marginally less fair.
    *
    * @param invertResult if true, invert the result so that 1 is best instead
    * @param minValue limit risk to at least this value, default 0.0
    * @param maxValue limit risk to as most this value, default 0.0
    */
  final case class AssignmentExternalities(
    invertResult: Boolean = false,
    minValue: Double = -1.0,
    maxValue: Double = 1.0
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

      case feature: ExperiencedDistancePercent =>
        for {
          hist <- IO.fromEither(hists.getAgentHistoryOrError(req.agent))
          req  <- IO.fromEither(hist.currentRequest)
          dist = req.experiencedDistance.value / req.overallDistance.value
          obs <- feature.mapFeatureRange(dist)
          _ = logger.debug(s"Distance - agent ${req.agent} value: $dist obs: $obs")
        } yield List(obs)

      case feature: FreeFlowOverTravelTimePercent =>
        for {
          hist      <- IO.fromEither(hists.getAgentHistoryOrError(req.agent))
          obsResult <- freeFlowOverTravelTimePercent(rn, hist)
          obs       <- feature.mapFeatureRange(obsResult)
          _ = logger.debug(s"Delay - agent ${req.agent} value: $obsResult obs: $obs")
        } yield List(obs)

      case feature: RiskOffset =>
        for {
          hist   <- IO.fromEither(hists.getAgentHistoryOrError(req.agent))
          uoSpur <- getUoPathAlternative(req, alts)
          soSpur <- getSoPathAlternative(req, alts)
          uoTime <- pathAlternativeTravelTimeEstimate(rn, hist, uoSpur)
          soTime <- pathAlternativeTravelTimeEstimate(rn, hist, soSpur)
          offset = if (uoTime == 0.0) 0.0 else (soTime - uoTime) / uoTime
          obs <- feature.mapFeatureRange(offset)
          _ = logger.debug(s"RiskOffset - agent ${req.agent} UO: $uoTime SO: $soTime offset: $offset obs: $obs")
          _ = logger.debug(s"  UO path spur: ${uoSpur.map { _.edgeId }.mkString("[", "->", "]")}")
          _ = logger.debug(s"  C  path spur: ${soSpur.map { _.edgeId }.mkString("[", "->", "]")}")
        } yield List(obs)

      case feature: BatchRisk =>
        // could be a config parameter if we explore alternatives like t-test
        val fn = BatchExternalitiesMetric.jainDiff
        for {
          batchRisk <- BatchFairnessExternalities.calculateBatchRisk(rn, alts, sig, hists, fn)
          obs       <- feature.mapFeatureRange(batchRisk.value)
        } yield List(obs)

      case feature: AssignmentExternalities =>
        for {
          result <- AssignmentFairnessExternality.calculate(rn, alts, hists, req.agent)
          obs    <- feature.mapFeatureRange(result)
          _ = logger.debug(s"AssignmentFairness - agent ${req.agent} value: $result obs: $obs")
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

      case feature: ExperiencedDistancePercent =>
        for { obs <- feature.mapFeatureRange(1.0) } yield List(obs)

      case feature: FreeFlowOverTravelTimePercent =>
        val pct      = freeFlowTravelTime.value.toDouble / finalTravelTime.value.toDouble
        val pctLimit = math.max(0.0, math.min(1.0, pct))
        for { obs <- feature.mapFeatureRange(pctLimit) } yield List(obs)

      case RiskOffset(invertResult, minOffset, maxOffset) =>
        val obs = if (invertResult) maxOffset else minOffset
        IO.pure(List(obs))

      case feature @ BatchRisk(invertResult, minRisk, maxRisk) =>
        // batch risk is non-monotonic, so a zero value is the observation of
        // no risk. this allows the user to override zero with some other minimum value.
        val zeroish = math.max(0.0, minRisk)
        for { obs <- feature.mapFeatureRange(zeroish) } yield List(obs)

      case feature: AssignmentExternalities => IO.pure(List(0.0))
    }

    def mapFeatureRange(value: Double): IO[Double] = {
      dps match {
        case Combined(features)                 => IO.raiseError(new Error(s"internal error, not defined for Combined type"))
        case ExperiencedDistancePercent(invert) => IO.pure(if (invert) invertPercent(value) else value)
        case FreeFlowOverTravelTimePercent(invert, max) =>
          val bounded = math.max(0.0, math.min(max, value))
          IO.pure(if (invert) invertValue(0.0, max, bounded) else value)
        case RiskOffset(invert, min, max) =>
          val bounded = math.max(min, math.min(max, value))
          IO.pure(if (invert) invertValue(min, max, bounded) else bounded)
        case BatchRisk(invert, min, max) =>
          val bounded = math.max(min, math.min(max, value))
          IO.pure(if (invert) invertValue(min, max, bounded) else bounded)
        case AssignmentExternalities(invert, min, max) =>
          val bounded = math.max(min, math.min(max, value))
          IO.pure(if (invert) invertValue(min, max, bounded) else bounded)
      }
    }
  }
}

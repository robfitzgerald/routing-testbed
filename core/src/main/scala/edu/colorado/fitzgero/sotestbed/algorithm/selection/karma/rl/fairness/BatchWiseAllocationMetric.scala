package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness._
import cats.implicits._
import cats.effect._
import java.nio.file.{Path => JavaNioPath}
import edu.colorado.fitzgero.sotestbed.rllib._
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Bid
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.implicits._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.DriverPolicySpaceV2Ops
import edu.colorado.fitzgero.sotestbed.model.numeric.Meters

sealed trait BatchWiseAllocationMetric

object BatchWiseAllocationMetric {

  case object NoBatchWiseAllocation extends BatchWiseAllocationMetric

  /**
    * observes the final difference between experienced and free flow
    * travel speed for a route. this is transformed into a percentage range
    * where 100% means the agent experienced no delay from free flow, and
    * 0% means the agent experienced an infinite delay, by using the travel
    * distance to transpose travel times to speeds, and then dividing the
    * free flow speed by the experienced speed.
    */
  case object FreeFlowDiffProportion extends BatchWiseAllocationMetric

  /**
    * only uses the marginal difference from the previous route to the newly-
    * assigned route
    */
  case object MarginalFreeFlowDiffProportion extends BatchWiseAllocationMetric

  /**
    * considers the protected resource to be "replannings" aka path updates.
    * reports the average distance per replanning event as replannings per trip distance (km).
    * if replannings is zero, reports an allocation of zero.
    *
    */
  case object ReplanningsPerUnitDistance extends BatchWiseAllocationMetric

  implicit class BWAMExtension(mwam: BatchWiseAllocationMetric) {

    /**
      * computes an allocation metric at the time of decision. when a
      * path alternative has been selected, we can estimate the allocation
      * for certain allocation metrics (not all) and use it in a batch-wise
      * fairness calculation.
      *
      * @param request the request to get the allocation for
      * @param selectedPathSpur (ksp) path selected for this agent
      * @param aah history of agent requests
      * @param rn road network state
      * @return effect of computing an allocation
      */
    def batchWiseAllocation(
      request: Request,
      selectedPathSpur: Option[Path],
      aah: ActiveAgentHistory,
      rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]
    ): IO[Option[Double]] = mwam match {
      case NoBatchWiseAllocation => IO.pure(None)
      case FreeFlowDiffProportion =>
        selectedPathSpur match {
          case Some(spur) =>
            getFutureFreeFlowAllocation(request, spur, aah, rn).map(alloc => Some(alloc))
          case None =>
            getCurrentFreeFlowAllocation(request, aah, rn).map { alloc => Some(alloc) }
        }

      case MarginalFreeFlowDiffProportion =>
        // values in the range [0, 2] from
        // - when future < current, we should lose allocation (100% - 120% = -20%)
        // - when future > current, we should gain allocation (100% - 80% = 20%)
        // - to move these values so they are non-negative (requirement for Jain's Index)
        //   - we add 1
        //   - we force the result to be greater than zero
        selectedPathSpur match {
          case None =>
            getCurrentFreeFlowAllocation(request, aah, rn).map { alloc => Some(alloc) }
          case Some(spur) =>
            for {
              current <- getCurrentFreeFlowAllocation(request, aah, rn)
              future  <- getFutureFreeFlowAllocation(request, spur, aah, rn)
              alloc = math.max(0.0, 1.0 + future - current)
            } yield Some(alloc)
        }

      case ReplanningsPerUnitDistance =>
        val newReplanning = if (selectedPathSpur.nonEmpty) 1 else 0
        for {
          hist <- IO.fromEither(aah.getAgentHistoryOrError(request.agent))
          replannings = hist.replanningEvents + newReplanning
          current <- IO.fromEither(hist.currentRequest)
        } yield
          if (current.experiencedDistance == Meters.Zero) None
          else {
            val distKm = current.experiencedDistance.value / 1000.0
            Some(replannings.toDouble / distKm)
          }
    }

  }

  /**
    * allocation of the agent's current route
    *
    * @param request agent request
    * @param aah history of route plans for all active agents
    * @param rn current road network state
    * @return "allocation" as speed/free flow speed of current route (including estimated segment)
    */
  def getCurrentFreeFlowAllocation(
    request: Request,
    aah: ActiveAgentHistory,
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]
  ): IO[Double] =
    for {
      hist       <- IO.fromEither(aah.getAgentHistoryOrError(request.agent))
      current    <- DriverPolicySpaceV2Ops.currentRoute(hist)
      currentReq <- IO.fromEither(hist.currentRequest)
      dists = current.map(_.linkDistance)
      tts <- DriverPolicySpaceV2Ops.travelTime(rn, currentReq.experiencedRoute, currentReq.remainingRoute)
      ffs <- DriverPolicySpaceV2Ops.freeFlowTravelTime(rn, current)
      dist = dists.foldLeft(0.0) { _ + _ }
      tt   = tts.foldLeft(0.0) { _ + _ }
      ff   = ffs.foldLeft(0.0) { _ + _ }
    } yield RewardOps.freeFlowSpeedDiffAllocation(tt, ff, dist)

  /**
    * allocation of the agent's route after applying a new path spur
    *
    * @param request agent request
    * @param selectedPathSpur new spur of route to apply to existing route
    * @param aah history of route plans for all active agents
    * @param rn current road network state
    * @return "allocation" as speed/free flow speed of current route with spur modification
    */
  def getFutureFreeFlowAllocation(
    request: Request,
    selectedPathSpur: Path,
    aah: ActiveAgentHistory,
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]
  ): IO[Double] =
    for {
      hist       <- IO.fromEither(aah.getAgentHistoryOrError(request.agent))
      current    <- DriverPolicySpaceV2Ops.currentRoute(hist)
      currentReq <- IO.fromEither(hist.currentRequest)
      spurEdges  <- selectedPathSpur.traverse(_.toEdgeDataButRetainCost(rn))
      remWithSpur = DriverPolicySpaceV2Ops.coalesceFuturePath(currentReq.remainingRoute, spurEdges)
      futurePath  = DriverPolicySpaceV2Ops.coalesceFuturePath(current, spurEdges)
      dists       = futurePath.map(_.linkDistance)
      tts <- DriverPolicySpaceV2Ops.travelTime(rn, currentReq.experiencedRoute, remWithSpur)
      ffs <- DriverPolicySpaceV2Ops.freeFlowTravelTime(rn, futurePath)
      dist = dists.foldLeft(0.0) { _ + _ }
      tt   = tts.foldLeft(0.0) { _ + _ }
      ff   = ffs.foldLeft(0.0) { _ + _ }
    } yield RewardOps.freeFlowSpeedDiffAllocation(tt, ff, dist)
}

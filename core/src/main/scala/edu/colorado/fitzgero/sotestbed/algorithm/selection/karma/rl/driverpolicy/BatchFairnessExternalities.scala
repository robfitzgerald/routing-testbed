package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness.JainFairnessMath
import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory

object BatchFairnessExternalities {

  // (UpperBounds, LowerBounds) => Result
  type BatchExternalitiesMetric = (List[Double], List[Double]) => IO[BatchExternalitiesResult]

  val jainDiff: BatchExternalitiesMetric = (best: List[Double], worst: List[Double]) => {
    val diffResult = for {
      ub <- JainFairnessMath.fairness(best)
      lb <- JainFairnessMath.fairness(worst)
    } yield BatchExternalitiesResult(ub - lb, lb, ub)
    IO.fromOption(diffResult)(new Error(s"unable to compute fairness of batch"))
  }

  val studentsTTest: BatchExternalitiesMetric = (best: List[Double], worst: List[Double]) => {
    ???
  }

  final case class BatchExternalitiesResult(
    value: Double,
    lowerBoundValue: Double,
    upperBoundValue: Double
  )

  def calculate(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    alts: Map[Request, List[Path]],
    sig: NetworkPolicySignal,
    hists: ActiveAgentHistory,
    calcFn: BatchExternalitiesMetric = jainDiff
  ): IO[BatchExternalitiesResult] = {

    import DriverPolicySpaceV2Ops._

    // extract the best and worst path alternative for each agent in this batch
    val bestWorstPathsResult = alts.toList
      .traverse {
        case (req, paths) =>
          val bestWorstOpt = for {
            best  <- paths.headOption
            worst <- paths.lastOption
          } yield (req, best, worst)
          IO.fromOption(bestWorstOpt)(new Error(s"agent ${req.agent} needs 2 paths, only has ${paths.length}"))
      }
    // evaluate the "allocation" for each, which is the free flow over travel
    // time percent value. these values are 1.0 if free flow equals travel time
    // and approach 0.0 as travel time increases, which make them suitable to be
    // used directly as allocation metrics.
    val bestWorstCostsResult =
      bestWorstPathsResult
        .flatMap {
          _.traverse {
            case (req, best, worst) =>
              for {
                bestEdges  <- best.traverse(_.toEdgeData(rn))
                worstEdges <- worst.traverse(_.toEdgeData(rn))
                hist       <- IO.fromEither(hists.getAgentHistoryOrError(req.agent))
                bestCost   <- freeFlowOverTravelTimePercent(rn, hist, bestEdges)
                worstCost  <- freeFlowOverTravelTimePercent(rn, hist, worstEdges)
              } yield (bestCost, worstCost)
          }
        }
    val calculatedResult =
      bestWorstCostsResult
        .flatMap { costs =>
          val num        = costs.length
          val numWinners = sig.estimatedWinners(num)
          val numLosers  = num - numWinners

          // create two collections, one where we assign most fairly
          // (upper bound fairness assignment) and one that is least
          // fairly assigned (lower bound). we sort ascending by the
          // worst allocations, so as we go, first $numWinners entries
          // would win most unfairly (lower bound).
          val (upperBound, lowerBound) = costs
            .sortBy { case (_, worst) => worst }
            .zipWithIndex
            .foldLeft((List.empty[Double], List.empty[Double])) {
              case ((ubAcc, lbAcc), ((best, worst), idx)) =>
                val nextUb = if (idx < numLosers) worst else best
                val nextLb = if (idx < numWinners) best else worst
                (nextUb +: ubAcc, nextLb +: lbAcc)
            }

          calcFn(upperBound, lowerBound)
        }

    calculatedResult
  }
}

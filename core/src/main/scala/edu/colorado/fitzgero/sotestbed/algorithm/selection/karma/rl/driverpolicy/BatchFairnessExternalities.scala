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
      diff          = ub - lb
      diffTruncated = math.min(1.0, math.max(0.0, diff))
    } yield BatchExternalitiesResult(diff, diffTruncated, lb, ub)
    IO.fromOption(diffResult)(new Error(s"unable to compute fairness of batch"))
  }

  val studentsTTest: BatchExternalitiesMetric = (best: List[Double], worst: List[Double]) => {
    ???
  }

  final case class BatchExternalitiesResult(
    difference: Double,
    differenceTruncated: Double,
    lowerBoundFairness: Double,
    upperBoundFairness: Double
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
    // time percent value
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
          val numWinners = sig.estimatedWinners(costs.length)
          // todo: build agent_ub, agent_lb
          val (upperBound, lowerBound) = costs
            .sortBy { case (_, worst) => worst }
            .zipWithIndex
            .foldLeft((List.empty[Double], List.empty[Double])) {
              case ((ubAcc, lbAcc), ((b, w), idx)) =>
                val nextUb = if (idx < numWinners) b else w
                val nextLb = if (idx >= numWinners) b else w
                (nextUb +: ubAcc, nextLb +: lbAcc)
            }

          calcFn(upperBound, lowerBound)
        }

    calculatedResult
  }
}

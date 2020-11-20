package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory.AgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, TravelTimeSeconds}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, PathSegment}

object KSPFilter {

  /**
    * a filter function which occurs between generating a set of alternative paths and selecting
    * an optimal combination from them. it takes the ksp result for an agent, the history of that
    * * agent's replanning, and a seed value for use with sampling-based approaches. it returns
    * any update to the alternative paths (such as removing long paths or only keeping the first 5
    * minutes of each path)
    */
  type KSPFilterFunction = (AgentHistory, Request, List[Path], Random) => Option[(Request, List[Path])]

  private[this] final case class CombinedKSPFilterFunctionAccumulator(
    history: AgentHistory,
    request: Request,
    alts: List[Path],
    random: Random
  )

  /**
    * builds a function which takes the ksp result for an agent, the history of that
    * agent's replanning, and a seed value for use with sampling-based approaches
    *
    * @return a combined filter function
    */
  def combine(kspFilterFunctions: List[KSPFilterFunction]): KSPFilterFunction = {

    (history: AgentHistory, request: Request, alts: List[Path], random: Random) =>
      {
        val initialAccumulator: Option[CombinedKSPFilterFunctionAccumulator] =
          Some { CombinedKSPFilterFunctionAccumulator(history, request, alts, random) }
        val finalAccumulator: Option[CombinedKSPFilterFunctionAccumulator] =
          kspFilterFunctions
            .foldLeft(initialAccumulator) { (accOpt, fn) =>
              for {
                acc                         <- accOpt
                (filteredReq, filteredAlts) <- fn(acc.history, acc.request, acc.alts, acc.random)
              } yield CombinedKSPFilterFunctionAccumulator(acc.history, filteredReq, filteredAlts, acc.random)
            }

        finalAccumulator.map { case CombinedKSPFilterFunctionAccumulator(_, finalReq, finalAlts, _) => (finalReq, finalAlts) }
      }
  }

  sealed trait LimitFunction {
    def limitPath(path: Path): Option[Path]
  }

  object LimitFunction {

    private[this] final case class ByTravelTimeAccumulator(reversePath: Path = List.empty, totalCost: Cost = Cost.Zero) {

      def addPathSegment(pathSegment: PathSegment): ByTravelTimeAccumulator =
        this.copy(
          reversePath = pathSegment +: this.reversePath,
          totalCost = this.totalCost + pathSegment.cost
        )
      def getPath: Path = this.reversePath.reverse
    }

    final case class ByTravelTime(travelTimeThreshold: TravelTimeSeconds) extends LimitFunction {

      def limitPath(path: Path): Option[Path] = {
        val accumulator: ByTravelTimeAccumulator =
          path.foldLeft(ByTravelTimeAccumulator()) { (acc, pathSegment) =>
            if (acc.totalCost.value + pathSegment.cost.value > travelTimeThreshold.value) acc
            else acc.addPathSegment(pathSegment)
          }
        val limitedPath: Path = accumulator.getPath
        if (limitedPath.isEmpty) None
        else Some { limitedPath }
      }
    }

    // at this point, PathSegment doesn't allow us to limit by distance, as we only have "cost"
    // which is equivalent to the travel time

  }
}

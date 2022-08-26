package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import scala.annotation.tailrec
import scala.util.Random

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentHistory
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

        val initialAccumulator: Option[(Request, List[Path])] = Some((request, alts))

        val result: Option[(Request, List[Path])] = kspFilterFunctions.foldLeft(initialAccumulator) { (acc, fn) =>
          val filterFunctionResult: Option[(Request, List[Path])] = for {
            (accReq, accAlts) <- acc
            result            <- fn(history, accReq, accAlts, random)
          } yield result
          filterFunctionResult
        }

        result
      }
  }

  sealed trait LimitFunction {
    def limitPath(path: Path): Option[Path]
  }

  object LimitFunction {

    /**
      * filters a path so that it only includes up to the provided accumulative travel time,
      * starting from the origin.
      *
      * @param travelTimeThreshold limit to the number of seconds of the path to filter
      */
    final case class ByTravelTime(travelTimeThreshold: TravelTimeSeconds) extends LimitFunction {

      def limitPath(path: Path): Option[Path] = {

        @tailrec
        def _limit(remaining: Path, accCost: Double = 0.0, reverseSolution: Path = List.empty): Path = {
          remaining match {
            case Nil =>
              reverseSolution
            case head :: tail =>
              val thisCost    = head.cost.value
              val nextAccCost = accCost + thisCost
              if (nextAccCost > travelTimeThreshold.value) {
                reverseSolution
              } else {
                val nextReverseSolution = head +: reverseSolution
                _limit(tail, nextAccCost, nextReverseSolution)
              }
          }
        }

        val limitResult = _limit(path).reverse
        if (limitResult.isEmpty) None else Some(limitResult)
      }
    }

    /**
      * filters a path so that it only includes up to the provided number of links
      * set in the linkCountThreshold, starting from the origin.
      *
      * @param linkCountThreshold limit to number of links in path
      */
    final case class ByLinkCount(linkCountThreshold: Int) extends LimitFunction {

      def limitPath(path: Path): Option[Path] = {
        val result = path.take(linkCountThreshold)
        Some(result)
      }
    }

  }
}

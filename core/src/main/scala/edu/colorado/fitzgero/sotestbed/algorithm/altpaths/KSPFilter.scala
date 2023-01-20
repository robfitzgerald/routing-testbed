package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import scala.annotation.tailrec
import scala.util.Random

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, TravelTimeSeconds}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, PathSegment}
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.DriverPolicySpaceV2Ops
import com.typesafe.scalalogging.LazyLogging

object KSPFilter extends LazyLogging {

  /**
    * a filter function which occurs between generating a set of alternative paths and selecting
    * an optimal combination from them. it takes the ksp result for an agent, the history of that
    * * agent's replanning, and a seed value for use with sampling-based approaches. it returns
    * any update to the alternative paths (such as removing long paths or only keeping the first 5
    * minutes of each path)
    */
  type KSPFilterFunction = (
    RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    AgentHistory,
    Request,
    List[Path],
    Random
  ) => IO[Option[(Request, List[Path])]]

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

    (
      rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
      history: AgentHistory,
      request: Request,
      alts: List[Path],
      random: Random
    ) =>
      {

        val initialAccumulator: IO[Option[(Request, List[Path])]] = IO.pure(Some((request, alts)))

        // todo: stack-safe me
        def _combine(
          acc: Option[(Request, List[Path])] = Some((request, alts)),
          remaining: List[KSPFilterFunction] = kspFilterFunctions
        ): IO[Option[(Request, List[Path])]] =
          acc match {
            case None => IO.pure(None)
            case Some((accReq, accPaths)) =>
              remaining match {
                case Nil => IO.pure(acc)
                case fn :: rest =>
                  fn(rn, history, accReq, accPaths, random)
                    .flatMap { result => _combine(result, rest) }
              }
          }

        _combine()
        // val result: Option[(Request, List[Path])] = kspFilterFunctions.foldLeft(initialAccumulator) { (acc, fn) =>
        //   val filterFunctionResult: Option[(Request, List[Path])] = for {
        //     (accReq, accAlts) <- acc
        //     result            <- fn(rn, history, accReq, accAlts, random)
        //   } yield result
        //   filterFunctionResult
        // }

        // result
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

  /**
    * after computing a batch of alternatives, we attach each to the current trip plan
    * and find out the sort ordering. each cost should estimate the full path plus
    * the path spur. the resulting sorted path lists make it so that the 0th path
    * is the most user-optimal, and the remaining paths appear in increasing system-
    * optimizing order, so that the last path is most-system-optimizing.
    *
    * @param altsResult the path alternatives to sort
    * @param activeAgentHistory dataset providing the current trip estimate for each agent
    * @return the path alternatives re-sorted
    */
  def pickUoAndSoPathFromPathAlternates(
    agentHistory: AgentHistory,
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    paths: List[Path],
    targetIncrease: Double
  ): IO[Option[List[Path]]] = {
    val pathsSortedByCostResult = paths
      .traverse { path =>
        DriverPolicySpaceV2Ops
          .pathAlternativeTravelTimeEstimate(rn, agentHistory, path)
          .map { cost => (path, cost) }
      }
      .map { _.sortBy { case (_, tt) => tt } }

    val result: IO[Option[List[Path]]] = pathsSortedByCostResult.map {
      case Nil                      => None
      case uo :: Nil                => None
      case (uoPath, uoCost) :: rest =>
        // the head path is the "UO option"
        // find the path alternative which is closest to the target increase from the UO option
        val targetMagnitude = (1.0 + targetIncrease) * uoCost
        val (soPath, soCost, distance) = rest
          .map { case (p, c) => (p, c, math.abs(c - targetMagnitude)) }
          .sortBy { case (p, c, dist) => dist }
          .head
        logger.info(
          f"filtered alt paths to UO/SO pair with costs $uoCost, $soCost (target increase ${targetIncrease * 100}%%"
        )
        val filtered = List(uoPath, soPath)
        Some(filtered)
    }

    result
  }
}

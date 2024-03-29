package edu.colorado.fitzgero.sotestbed.config

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPFilter
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPFilter.{KSPFilterFunction, LimitFunction}
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Meters, TravelTimeSeconds}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import cats.effect.IO
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR

/**
  * a filter function which occurs between generating a set of alternative paths and selecting
  * an optimal combination from them.
  */
sealed trait KSPFilterFunctionConfig {

  /**
    * builds a function which takes the ksp result for an agent, the history of that
    * agent's replanning, and a seed value for use with sampling-based approaches
    * @return the request filtered
    */
  def build(): KSPFilterFunction
}

object KSPFilterFunctionConfig {

  /**
    * the NOOP filter function
    */
  final case object NoFilter extends KSPFilterFunctionConfig {

    /**
      * builds a function which takes the ksp result for an agent, the history of that
      * agent's replanning, and a seed value for use with sampling-based approaches
      *
      * @return the request filtered
      */
    def build(): KSPFilterFunction =
      (
        _: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
        _: AgentHistory,
        request: Request,
        alts: List[Path],
        _: Random
      ) => {
        IO.pure(Some((request, alts)))
      }
  }

  /**
    * only accepts KSP results which do not re-visit edges that the agent has
    * already traversed, up to some maxEdgeVisits per edge. at 1, this would
    * force the path to have zero cycles, and at 2, it could have many cycles,
    * but possibly be more robust to changes in the network state.
    *
    * @param maxEdgeVisits the number of times an agent can re-visit the same edge
    */
  final case class LimitedEdgeVisits(maxEdgeVisits: Int) extends KSPFilterFunctionConfig {

    /**
      * builds a function which takes the ksp result for an agent, the history of that
      * agent's replanning, and a seed value for use with sampling-based approaches
      *
      * @return the request filtered
      */
    def build(): KSPFilterFunction =
      (
        _: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
        history: AgentHistory,
        request: Request,
        alts: List[Path],
        _: Random
      ) => {

        // a mapping from edges to the number of times those edges exist in the experienced route
        val experiencedRouteSet: Map[EdgeId, Int] =
          history.currentRequest.toOption
            .map { _.experiencedRoute }
            .getOrElse(List.empty)
            .map { _.edgeId }
            .foldLeft(Map.empty[EdgeId, Int]) { (acc, edgeId) =>
              val edgeVisitCount: Int = acc.getOrElse(edgeId, 0)
              acc.updated(edgeId, edgeVisitCount + 1)
            }

        // filter only alternatve paths which do not exceed the maxEdgeVisits threshold for all edges
        val acceptableAltPaths: List[Path] =
          alts.filter { alt =>
            alt.forall { pathSegment =>
              val previouslyVisitedCount: Int = experiencedRouteSet.getOrElse(pathSegment.edgeId, 0)
              previouslyVisitedCount < maxEdgeVisits
            }
          }

        if (acceptableAltPaths.isEmpty) IO.pure(None)
        else IO.pure(Some((request, acceptableAltPaths)))
      }
  }

  /**
    * looks at how much farther the agent has to travel as a means to probabilistically sample
    * whether to allow SO replanning.
    *
    * intuition: as the agent gets closer to their destination, we should replan them less frequently.
    */
  final case object SampleFromRemainingDistanceProportion extends KSPFilterFunctionConfig {

    /**
      * builds a function which takes the ksp result for an agent, the history of that
      * agent's replanning, and a seed value for use with sampling-based approaches
      *
      * @return the request filtered
      */
    def build(): KSPFilterFunction =
      (
        _: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
        history: AgentHistory,
        request: Request,
        alts: List[Path],
        random: Random
      ) => {
        val originalDistance: Meters   = history.first.tripDistance
        val currentDistance: Meters    = history.currentRequest.toOption.map { _.remainingDistance }.getOrElse(Meters.Zero)
        val proportion: Double         = math.min(1.0, math.max(0.0, currentDistance.value / originalDistance.value))
        val allowSOReplanning: Boolean = random.nextDouble() < proportion
        if (allowSOReplanning) IO.pure(Some((request, alts)))
        else IO.pure(Some((request, alts.take(1))))
      }
  }

  final case class CombinedLimitAndSample(maxEdgeVisits: Int) extends KSPFilterFunctionConfig {

    val limitFn: KSPFilterFunction  = LimitedEdgeVisits(maxEdgeVisits).build()
    val sampleFn: KSPFilterFunction = SampleFromRemainingDistanceProportion.build()

    /**
      * builds a function which takes the ksp result for an agent, the history of that
      * agent's replanning, and a seed value for use with sampling-based approaches
      *
      * @return the request filtered
      */
    def build(): KSPFilterFunction =
      (
        rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
        history: AgentHistory,
        request: Request,
        alts: List[Path],
        random: Random
      ) => {
        limitFn(rn, history, request, alts, random).flatMap {
          case None                        => IO.pure(None)
          case Some((limitReq, limitAlts)) => sampleFn(rn, history, limitReq, limitAlts, random)
        }
      }
  }

  final case class LimitPath(limitFunction: LimitFunction) extends KSPFilterFunctionConfig {

    /**
      * builds a function which takes the ksp result for an agent, the history of that
      * agent's replanning, and a seed value for use with sampling-based approaches
      *
      * @return the request filtered
      */
    def build(): KSPFilterFunction =
      (
        _: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
        _: AgentHistory,
        req: Request,
        paths: List[Path],
        _: Random
      ) => {
        val limitedPaths: List[Path] = for {
          path        <- paths
          limitedPath <- limitFunction.limitPath(path)
        } yield limitedPath
        val result: Option[(Request, List[Path])] =
          if (limitedPaths.isEmpty) None
          else Some { (req, limitedPaths) }
        IO.pure(result)
      }
  }

  final case class LimSampLim(maxEdgeVisits: Int, travelTimeThreshold: Double) extends KSPFilterFunctionConfig {

    val fns: List[KSPFilterFunction] = List(
      LimitedEdgeVisits(maxEdgeVisits).build(),
      SampleFromRemainingDistanceProportion.build(),
      LimitPath(KSPFilter.LimitFunction.ByTravelTime(TravelTimeSeconds(travelTimeThreshold))).build()
    )

    /**
      * builds a function which takes the ksp result for an agent, the history of that
      * agent's replanning, and a seed value for use with sampling-based approaches
      *
      * @return the request filtered
      */
    def build(): KSPFilterFunction = KSPFilter.combine(fns)

  }

  final case class TravelTime(travelTimeThreshold: Double) extends KSPFilterFunctionConfig {

    override def build(): KSPFilterFunction =
      LimitPath(
        KSPFilter.LimitFunction.ByTravelTime(TravelTimeSeconds(travelTimeThreshold))
      ).build()
  }

  final case class TravelTimeAndLinkCount(maxEdgeVisits: Int, travelTimeThreshold: Double, linkCount: Int)
      extends KSPFilterFunctionConfig {

    val fns: List[KSPFilterFunction] = List(
      LimitedEdgeVisits(maxEdgeVisits).build(),
      SampleFromRemainingDistanceProportion.build(),
      LimitPath(KSPFilter.LimitFunction.ByLinkCount(linkCount)).build(),
      LimitPath(KSPFilter.LimitFunction.ByTravelTime(TravelTimeSeconds(travelTimeThreshold))).build()
    )

    /**
      * builds a function which takes the ksp result for an agent, the history of that
      * agent's replanning, and a seed value for use with sampling-based approaches
      *
      * @return the request filtered
      */
    def build(): KSPFilterFunction = KSPFilter.combine(fns)

  }

  final case class PickUoAndSoPath(targetIncrease: Double) extends KSPFilterFunctionConfig {

    def build(): KSPFilterFunction =
      (
        rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
        hist: AgentHistory,
        req: Request,
        paths: List[Path],
        rng: Random
      ) => {
        KSPFilter
          .pickUoAndSoPathFromPathAlternates(hist, rn, paths, targetIncrease)
          .map { _.map { filteredPaths => (req, filteredPaths) } }
      }

  }
}

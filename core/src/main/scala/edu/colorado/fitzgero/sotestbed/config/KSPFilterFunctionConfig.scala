package edu.colorado.fitzgero.sotestbed.config

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPFilter
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPFilter.{KSPFilterFunction, LimitFunction}
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory.AgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Meters, TravelTimeSeconds}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path}

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
  final case object DoNothing extends KSPFilterFunctionConfig {

    /**
      * builds a function which takes the ksp result for an agent, the history of that
      * agent's replanning, and a seed value for use with sampling-based approaches
      *
      * @return the request filtered
      */
    def build(): KSPFilterFunction =
      (_: AgentHistory, request: Request, alts: List[Path], _: Random) => {
        Some { (request, alts) }
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
      (history: AgentHistory, request: Request, alts: List[Path], _: Random) => {

        // a mapping from edges to the number of times those edges exist in the experienced route
        val experiencedRouteSet: Map[EdgeId, Int] =
          history.last.experiencedRoute
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

        if (acceptableAltPaths.isEmpty) None
        else Some { (request, acceptableAltPaths) }
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
      (history: AgentHistory, request: Request, alts: List[Path], random: Random) => {
        val originalDistance: Meters   = history.first.remainingRouteDistance
        val currentDistance: Meters    = history.last.remainingRouteDistance
        val proportion: Double         = math.min(1.0, math.max(0.0, currentDistance.value / originalDistance.value))
        val allowSOReplanning: Boolean = random.nextDouble() < proportion
        if (allowSOReplanning) Some { (request, alts) }
        else Some { (request, alts.take(1)) }
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
      (history: AgentHistory, request: Request, alts: List[Path], random: Random) => {
        for {
          (limitReq, limitAlts) <- limitFn(history, request, alts, random)
          result                <- sampleFn(history, limitReq, limitAlts, random)
        } yield result
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
      (_: AgentHistory, req: Request, paths: List[Path], _: Random) => {
        val limitedPaths: List[Path] = for {
          path        <- paths
          limitedPath <- limitFunction.limitPath(path)
        } yield limitedPath
        val result: Option[(Request, List[Path])] =
          if (limitedPaths.isEmpty) None
          else Some { (req, limitedPaths) }
        result
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
}

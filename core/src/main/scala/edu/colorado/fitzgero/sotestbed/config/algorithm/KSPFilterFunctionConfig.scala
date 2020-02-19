package edu.colorado.fitzgero.sotestbed.config.algorithm

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory.AgentHistory
import edu.colorado.fitzgero.sotestbed.config.algorithm.KSPFilterFunctionConfig.KSPFilterFunction
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Meters
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

  type KSPFilterFunction = (AgentHistory, Request, List[Path], Random) => Option[(Request, List[Path])]

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

//  /**
//    * allows the user to configure an ordered sequence of ksp filter functions
//    * @param kspFilterFunctionConfigs the filter functions to apply to each ksp result
//    */
//  final case class Combined(kspFilterFunctionConfigs: List[KSPFilterFunctionConfig]) extends KSPFilterFunctionConfig {
//
//    final case class Accumulator(
//      history: AgentHistory,
//      request: Request,
//      alts: List[Path],
//      random: Random
//    )
//
//    /**
//      * builds a function which takes the ksp result for an agent, the history of that
//      * agent's replanning, and a seed value for use with sampling-based approaches
//      *
//      * @return the request filtered
//      */
//    def build(): KSPFilterFunction = {
//      val kspFilterFunctions: List[KSPFilterFunction] = kspFilterFunctionConfigs.map{_.build()}
//      (history: AgentHistory, request: Request, alts: List[Path], random: Random) => {
//        val initialAccumulator: Option[Accumulator] =
//          Some{ Accumulator(history, request, alts, random) }
//        val finalAccumulator: Option[Accumulator] =
//          kspFilterFunctions
//            .foldLeft(initialAccumulator) { (accOpt, fn) =>
//              for {
//                acc <- accOpt
//                (filteredReq, filteredAlts) <- fn(acc.history, acc.request, acc.alts, acc.random)
//              } yield Accumulator(acc.history, filteredReq, filteredAlts, acc.random)
//            }
//
//        finalAccumulator.map{ case Accumulator(_, finalReq, finalAlts, _) => (finalReq, finalAlts) }
//      }
//    }
//  }

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
        val experiencedRouteSet : Map[EdgeId, Int] =
          history
            .last
            .experiencedRoute.map{_.edgeId}
            .foldLeft(Map.empty[EdgeId, Int]) { (acc, edgeId) =>
              val edgeVisitCount: Int = acc.getOrElse(edgeId, 0)
              acc.updated(edgeId, edgeVisitCount + 1)
            }

        // filter only alternatve paths which do not exceed the maxEdgeVisits threshold for all edges
        val acceptableAltPaths: List[Path] =
          alts.filter{ alt =>
            alt.forall{ pathSegment =>
              val previouslyVisitedCount: Int = experiencedRouteSet.getOrElse(pathSegment.edgeId, 0)
              previouslyVisitedCount < maxEdgeVisits
            }
          }

        if (acceptableAltPaths.length < alts.length) {
          println("un axx eptbl")
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
        val originalDistance: Meters = history.head.remainingRouteDistance
        val currentDistance: Meters = history.last.remainingRouteDistance
        val proportion: Double = math.min(1.0, math.max(0.0, currentDistance.value / originalDistance.value))
        val allowSOReplanning: Boolean = random.nextDouble() < proportion
        if (allowSOReplanning) Some{ (request, alts) }
        else Some { (request, alts.take(1)) }
      }
  }

  final case class CombinedLimitAndSample(maxEdgeVisits: Int) extends KSPFilterFunctionConfig {

    val limitFn: KSPFilterFunction = LimitedEdgeVisits(maxEdgeVisits).build()
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
          result <- sampleFn(history, limitReq, limitAlts, random)
        } yield result
      }
  }
}
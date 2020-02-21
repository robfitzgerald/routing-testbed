package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory.AgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path

object KSPFilter {

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

    (history: AgentHistory, request: Request, alts: List[Path], random: Random) => {
      val initialAccumulator: Option[CombinedKSPFilterFunctionAccumulator] =
        Some{ CombinedKSPFilterFunctionAccumulator(history, request, alts, random) }
      val finalAccumulator: Option[CombinedKSPFilterFunctionAccumulator] =
        kspFilterFunctions
          .foldLeft(initialAccumulator) { (accOpt, fn) =>
            for {
              acc <- accOpt
              (filteredReq, filteredAlts) <- fn(acc.history, acc.request, acc.alts, acc.random)
            } yield CombinedKSPFilterFunctionAccumulator(acc.history, filteredReq, filteredAlts, acc.random)
          }

      finalAccumulator.map{ case CombinedKSPFilterFunctionAccumulator(_, finalReq, finalAlts, _) => (finalReq, finalAlts) }
    }
  }
}

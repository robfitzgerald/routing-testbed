package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import scala.util.Random

import cats.Monad
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPFilter.KSPFilterFunction
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}

final case class AltPathsAlgorithmRunner[F[_]: Monad, V, E](
  altPathsAlgorithm: KSPAlgorithm[F, V, E],
  kspFilterFunction: KSPFilterFunction,
  costFunction: E => Cost,
  marginalCostFunction: E => Flow => Cost,
  useFreeFlowNetworkCostsInPathSearch: Boolean,
  seed: Long
) extends LazyLogging {

  def run(
    batchId: String,
    reqs: List[Request],
    activeAgentHistory: ActiveAgentHistory,
    roadNetwork: RoadNetwork[F, V, E]
  ): F[AltPathsAlgorithmResult] = {

    val startTime: RunTime = RunTime(System.currentTimeMillis)

    val rng: Random = new Random(seed)

//    val costFunction: E => Cost =
//      if (useFreeFlowNetworkCostsInPathSearch) e => marginalCostFunction(e)(Flow.Zero)
//      else e => marginalCostFunction(e)(e.flow)

    for {
      altsResult <- altPathsAlgorithm.generateAlts(reqs, roadNetwork, costFunction)
    } yield {
      // first, apply the ksp filter function
      val altsWithFilterApplied: Map[Request, List[Path]] = altsResult.alternatives.flatMap {
        case (req, alts) =>
          activeAgentHistory.observedRouteRequestData.get(req.agent) match {
            case None =>
              logger.warn(f"agent ${req.agent} with alts has no AgentHistory")
              Some {
                req -> alts
              }
            case Some(agentHistory) =>
              kspFilterFunction(agentHistory, req, alts, rng) match {
                case None =>
                  logger.debug(f"ksp filter fn removed agent ${req.agent}")
                  None
                case Some(filtered) =>
                  logger.debug(f"ksp filter processed agent ${req.agent}")
                  Some(filtered)
              }
          }
      }

      val filteredAlts: Option[Map[Request, List[Path]]] =
        if (altsWithFilterApplied.isEmpty) None else Some(altsWithFilterApplied)

      val endOfKspTime = startTime - RunTime(System.currentTimeMillis)
      AltPathsAlgorithmResult(batchId, altsResult.alternatives, filteredAlts, endOfKspTime)
    }
  }
}

object AltPathsAlgorithmRunner {

  final case class AltPathsAlgorithmResult(
    batchId: String,
    alts: Map[Request, List[Path]],
    filteredAlts: Option[Map[Request, List[Path]]],
    runtimeMilliseconds: RunTime
  )
}

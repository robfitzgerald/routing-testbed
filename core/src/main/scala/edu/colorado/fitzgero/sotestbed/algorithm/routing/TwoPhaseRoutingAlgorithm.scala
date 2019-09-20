package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats._
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.RunTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

class TwoPhaseRoutingAlgorithm[F[_] : Monad, V, E] (
  altPathsAlgorithm: AltPathsAlgorithm[F, V, E],
  selectionAlgorithm: SelectionAlgorithm[F, V, E]
) extends RoutingAlgorithm[F, V, E] {

  def timeLimit: RunTime             = RunTime(31536000) // one year.
  def limitAltsRuntime: Boolean      = true
  def limitSelectionRuntime: Boolean = true

  final override def route(reqs       : List[Request],
                           roadNetwork: RoadNetwork[F, V, E]): F[RoutingAlgorithm.Result] = {

    val startTime: RunTime = RunTime(System.currentTimeMillis)
    val endTime: Option[RunTime] = if (limitAltsRuntime) {
      Some {
        startTime + timeLimit
      }
    } else None

    for {
      altsResult <- altPathsAlgorithm.generateAlts(reqs, roadNetwork, endTime)
      (alts, altsRuntime) = altsResult
      selectionResult <- selectionAlgorithm.selectRoutes(alts, roadNetwork, endTime)
      (selection, selectionRuntime) = selectionResult
    } yield {
      RoutingAlgorithm.Result(
        selection,
        altsRuntime,
        selectionRuntime
      )
    }
  }
}

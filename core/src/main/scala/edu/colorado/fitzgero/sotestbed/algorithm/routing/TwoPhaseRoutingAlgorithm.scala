package edu.colorado.fitzgero.sotestbed.algorithm.routing
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.RunTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetworkState

trait TwoPhaseRoutingAlgorithm[V, E]
    extends RoutingAlgorithm[V, E]
    with AltPathsAlgorithm[V, E]
    with SelectionAlgorithm[V, E] { self =>

  def timeLimit: RunTime             = RunTime(31536000) // one year.
  def limitAltsRuntime: Boolean      = true
  def limitSelectionRuntime: Boolean = true

  final override def route(reqs: List[Request],
                           roadNetworkModel: RoadNetworkState[V, E]): RoutingAlgorithm.Result = {

    val (alts, altsRuntime, altsEndTimeOption) = if (limitAltsRuntime) {
      val startTime: RunTime = RunTime(System.currentTimeMillis)
      val endTime: RunTime   = startTime + timeLimit
      val (alts, runtime)    = self.generateAlts(reqs, roadNetworkModel, Some { endTime })
      (alts, runtime, Some { startTime })
    } else {
      val (alts, runtime) = self.generateAlts(reqs, roadNetworkModel, None)
      (alts, runtime, Option.empty[RunTime])
    }

    val selectionEndTime: Option[RunTime] =
      if (limitSelectionRuntime) {
        altsEndTimeOption.orElse(Some {
          RunTime(System.currentTimeMillis)
        })
      } else {
        None
      }

    val (selection, selectionRuntime) =
      self.selectRoutes(alts, roadNetworkModel, selectionEndTime)

    RoutingAlgorithm.Result(
      selection,
      altsRuntime,
      selectionRuntime
    )
  }
}

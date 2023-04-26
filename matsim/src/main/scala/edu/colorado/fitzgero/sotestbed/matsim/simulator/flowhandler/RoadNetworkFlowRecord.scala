package edu.colorado.fitzgero.sotestbed.matsim.simulator.flowhandler

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import org.matsim.api.core.v01.Id
import org.matsim.vehicles.Vehicle
import edu.colorado.fitzgero.sotestbed.matsim.simulator.flowhandler.RoadNetworkLinkTraversal

// we want to store the agent's id and the start time when they enter a link
// we want to transform the stored entry into an observation of time duration ordered
// by start time when the agent leaves
// we want to make it easy to average the observations over a window duration
// we want to store the most recently-computed average
// if there are no flows we want to know to compute free flow travel times instead
// we want it to be easy to remove observations outside of the time window
// is the time window here the same as the network update window? or should it be longer?

/**
  * stores information about the flows and observed traversal times for a link and provides
  * a method to collect the average traversal time over some history
  *
  * @param currentAgentCount
  * @param linkEnterTimes
  * @param completedTraversals
  * @param mostRecentAverageTraversalDurationSeconds
  */
final case class RoadNetworkFlowRecord(
  currentAgentCount: Int = 0,
  linkEnterTimes: Map[Id[Vehicle], SimTime] = Map.empty,
  completedTraversals: List[RoadNetworkLinkTraversal] = List.empty,
  mostRecentAverageTraversalDurationSeconds: Option[Double] = None
) {

  /**
    * convenience method that performs all processing associated with collecting network update data
    *
    * @param binStartInclusive start of the time horizon to sample trip durations from
    * @return the observations based on the current state and the updated flow record with stale trips
    */
  def updateAndCollect(binStartInclusive: SimTime): (RoadNetworkFlowRecord, RoadNetworkFlowObservation) = {
    val updated  = this.computeAverageTraversalDurationSeconds(binStartInclusive)
    val duration = updated.getAverageTraversalDurationSeconds(binStartInclusive)
    (updated, RoadNetworkFlowObservation(this.getFlowCount, duration))
  }

  override def toString: String =
    f"(flow=${this.getFlowCount},avgDur=${this.mostRecentAverageTraversalDurationSeconds})"

  def getFlowCount: Int = this.currentAgentCount

  def getAverageTraversalDurationSeconds(binStartInclusive: SimTime): Option[Double] =
    if (currentAgentCount == 0) None
    else this.mostRecentAverageTraversalDurationSeconds

  def computeAverageTraversalDurationSeconds(binStartInclusive: SimTime): RoadNetworkFlowRecord = {
    val (acc, cnt) = this.completedTraversals.foldLeft((0.0, 0)) {
      case ((acc, cnt), t) =>
        if (t.startTime < binStartInclusive) (acc, cnt) else (acc + t.duration.value.toDouble, cnt + 1)
    }
    val avg = if (cnt == 0) None else Some(acc / cnt)
    this.copy(
      mostRecentAverageTraversalDurationSeconds = avg,
      completedTraversals = this.completedTraversals.filter(_.startTime >= binStartInclusive)
    )
  }

  def processLinkEnter(vehicleId: Id[Vehicle], enterTime: SimTime): RoadNetworkFlowRecord =
    this.copy(
      currentAgentCount = this.currentAgentCount + 1,
      linkEnterTimes = this.linkEnterTimes.updated(vehicleId, enterTime)
    )

  def processLinkExit(vehicleId: Id[Vehicle], exitTime: SimTime): RoadNetworkFlowRecord =
    this.linkEnterTimes.get(vehicleId) match {
      case None => this
      case Some(enterTime) =>
        val duration  = exitTime - enterTime
        val traversal = RoadNetworkLinkTraversal(enterTime, duration)
        this.copy(
          currentAgentCount = this.currentAgentCount - 1,
          linkEnterTimes = this.linkEnterTimes.removed(vehicleId),
          completedTraversals = traversal +: this.completedTraversals
        )
    }
}

package edu.colorado.fitzgero.sotestbed.matsim.simulator.flowhandler

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import org.matsim.api.core.v01.Id
import org.matsim.vehicles.Vehicle
import edu.colorado.fitzgero.sotestbed.matsim.simulator.flowhandler.RoadNetworkLinkTraversal
import edu.colorado.fitzgero.sotestbed.model.numeric.MetersPerSecond
import edu.colorado.fitzgero.sotestbed.model.numeric.Meters
import edu.colorado.fitzgero.sotestbed.model.numeric.TravelTimeSeconds
import com.typesafe.scalalogging.LazyLogging

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
  * data is stored such that the time of query isn't relevant. this allows
  * the query timing window does not need to match nor sync with the binStartTime (allowing
  * for overlap, such as a 5-minute history checked every 2 minutes). however it is
  * assumed that both advance linearly with static step sizes.
  *
  * @param currentAgentCount
  * @param linkEnterTimes
  * @param completedTraversals
  * @param mostRecentAverageTraversalDurationSeconds
  */
final case class RoadNetworkFlowRecord(
  linkLengthMeters: Double,
  currentAgentCount: Int = 0,
  linkEnterTimes: Map[Id[Vehicle], RoadNetworkActiveTraversal] = Map.empty,
  completedTraversals: List[RoadNetworkLinkTraversal] = List.empty,
  mostRecentAverageTraversalDurationSeconds: Option[Double] = None,
  mostRecentAverageTraversalSpeedMps: Option[Double] = None
) extends LazyLogging {

  /**
    * convenience method that performs all processing associated with collecting network update data
    * and advancing the bin model to some new start time.
    *
    * @param binStartInclusive start of the time horizon to sample trip durations from
    * @return the observations based on the current state and the updated flow record with stale trips
    */
  def updateAndCollect(binStartInclusive: SimTime): (RoadNetworkFlowRecord, RoadNetworkFlowObservation) = {
    val updated  = this.computeNewAverages(binStartInclusive)
    val duration = updated.mostRecentAverageTraversalDurationSeconds
    val speed    = updated.mostRecentAverageTraversalSpeedMps
    val cleaned  = updated.clearObservationsBefore(binStartInclusive)
    (cleaned, RoadNetworkFlowObservation(cleaned.getFlowCount, duration, speed))
  }

  override def toString: String =
    f"(flow=${this.getFlowCount},avgDur=${this.mostRecentAverageTraversalDurationSeconds})"

  def getFlowCount: Int = this.currentAgentCount

  /**
    * computes new average observed travel time and speed values for this record.
    * only reports full link traversals, no partial link traversals
    *
    * @param binStartInclusive time in simulation to use as a lower bound for the inclusion of recent trips
    * @param minSpeedMps minimum allowed speed value to report for a given traversal
    * @return the updated record with new averages cached on it
    */
  def computeNewAverages(binStartInclusive: SimTime, minSpeedMps: Double = 1.0): RoadNetworkFlowRecord = {
    val (ttSum, spdSum, cnt) = this.completedTraversals
      .filter(_.fullLinkTraversal) // why record partial traversals in the first place then
      .foldLeft((0.0, 0.0, 0)) {
        case ((ttAcc, spdAcc, cnt), t) =>
          if (t.startTime < binStartInclusive) (ttAcc, spdAcc, cnt)
          else {
            val relativeLength = (t.endPos.value - t.startPos.value) * this.linkLengthMeters
            val speed          = relativeLength / t.duration.value.toDouble
            val speedSafe      = if (speed.isInfinite) minSpeedMps else math.max(minSpeedMps, speed)
            (ttAcc + t.duration.value.toDouble, spdAcc + speedSafe, cnt + 1)
            // if (t.duration.value >= relLength) {
            //   // don't record speeds when the speed would be less than 1 meter per second
            //   (ttAcc + t.duration.value.toDouble, spdAcc, cnt + 1)
            // } else {
            //   val speed = MetersPerSecond(Meters(relLength), TravelTimeSeconds(t.duration.value.toDouble)).value
            //   (ttAcc + t.duration.value.toDouble, spdAcc + speed, cnt + 1)
            // }
          }
      }
    // computes the next average, based on the following conditions:
    // 1. no agents on the link? no estimate (fall back on free flow)
    // 2. agents are on the link but no trips were completed? carry over the previous estimate
    //    - this covers the (hopefully rare) case that the traversals are taking longer than the window itself
    // 3. there are observations from which we can compute a new average
    val newTT: Option[Double] =
      if (currentAgentCount == 0 && cnt == 0) None
      else if (cnt == 0) this.mostRecentAverageTraversalDurationSeconds
      else Some(ttSum / cnt)

    val newSpeed: Option[Double] =
      if (currentAgentCount == 0 && cnt == 0) None
      else if (cnt == 0) this.mostRecentAverageTraversalSpeedMps
      else Some(spdSum / cnt)

    if (newSpeed.exists(s => s == Double.PositiveInfinity || s == Double.NegativeInfinity)) {
      val traversals = this.completedTraversals.map(_.toString)
      val header     = f"inf speed observed with avg travel time $newTT"
      val msg        = traversals.mkString(header + "\n", ",\n", "")
      logger.warn(msg)
    }

    this.copy(
      mostRecentAverageTraversalDurationSeconds = newTT,
      mostRecentAverageTraversalSpeedMps = newSpeed
    )
  }

  def clearObservationsBefore(binStartInclusive: SimTime): RoadNetworkFlowRecord =
    this.copy(completedTraversals = this.completedTraversals.filter(_.startTime >= binStartInclusive))

  def processLinkEnter(
    vehicleId: Id[Vehicle],
    enterTime: SimTime,
    position: Option[LinkPosition]
  ): RoadNetworkFlowRecord = {
    val traversal = RoadNetworkActiveTraversal(enterTime, position.getOrElse(LinkPosition.Start))
    this.copy(
      currentAgentCount = this.currentAgentCount + 1,
      linkEnterTimes = this.linkEnterTimes.updated(vehicleId, traversal)
    )
  }

  def processLinkExit(
    vehicleId: Id[Vehicle],
    exitTime: SimTime,
    position: Option[LinkPosition]
  ): RoadNetworkFlowRecord =
    this.linkEnterTimes.get(vehicleId) match {
      case None => this
      case Some(RoadNetworkActiveTraversal(enterTime, startPosition)) =>
        val duration    = exitTime - enterTime
        val endPosition = position.getOrElse(LinkPosition.End)
        val traversal   = RoadNetworkLinkTraversal(enterTime, duration, startPosition, endPosition)
        this.copy(
          currentAgentCount = this.currentAgentCount - 1,
          linkEnterTimes = this.linkEnterTimes.removed(vehicleId),
          completedTraversals = traversal +: this.completedTraversals
        )
    }

  def removeActiveTraversal(vehicleId: Id[Vehicle]): RoadNetworkFlowRecord =
    this.copy(linkEnterTimes = linkEnterTimes.removed(vehicleId))
}

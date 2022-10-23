package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import cats.effect.IO
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork

sealed trait ObservationAggregation

object ObservationAggregation {

  val MinSpeedMph             = 5.0
  val MinSpeedMetersPerSecond = (MinSpeedMph / 3600.0) * 1609.0

  /**
    * average of the directly observed current speed values
    */
  case object MeanCurrentSpeed extends ObservationAggregation

  /**
    * average of the directly observed current speed values using a link distance-weighted mean aggregation
    */
  case object MeanDistanceWeightedCurrentSpeed extends ObservationAggregation

  /**
    * average of directly observed speed diff values
    */
  case object MeanAbsoluteSpeedDiff extends ObservationAggregation

  /**
    * average of directly observed speed diff values using a link distance-weighted mean aggregation
    */
  case object MeanAbsoluteDistanceWeightedSpeedDiff extends ObservationAggregation

  /**
    * average of normalized speed diff values
    */
  case object MeanRelativeSpeedDiff extends ObservationAggregation

  /**
    * average of normalized speed diff values using a link distance-weighted mean aggregation
    */
  case object MeanRelativeDistanceWeightedSpeedDiff extends ObservationAggregation

  implicit class OAExtensions(oa: ObservationAggregation) {

    /**
      * given a zone of network edges, apply some variant of an ObservationAggregation
      * function, returning a single value as a result. makes a healthy assumption that
      * distances are strictly positive.
      *
      * @param obs the observed zone of edges
      * @return the aggregated observation value
      */
    def aggregate(obs: List[EdgeBPR]): Double = {
      if (obs.isEmpty) 0.0
      else
        oa match {

          case MeanCurrentSpeed =>
            val speeds = obs.map(absoluteSpeedObservation)
            speeds.sum / speeds.size

          case MeanDistanceWeightedCurrentSpeed =>
            val obsFn: EdgeBPR => Double = distanceWeightedObservation(absoluteSpeedObservation)
            obs.map(obsFn).sum / distancesSum(obs)

          case MeanAbsoluteSpeedDiff =>
            val diffs = obs.map(absoluteSpeedDiff)
            diffs.sum / diffs.size

          case MeanAbsoluteDistanceWeightedSpeedDiff =>
            val obsFn: EdgeBPR => Double = distanceWeightedObservation(absoluteSpeedDiff)
            obs.map(obsFn).sum / distancesSum(obs)

          case MeanRelativeSpeedDiff =>
            val diffs = obs.map(relativeSpeedDiff)
            diffs.sum / diffs.size

          case MeanRelativeDistanceWeightedSpeedDiff =>
            val obsFn: EdgeBPR => Double = distanceWeightedObservation(relativeSpeedDiff)
            obs.map(obsFn).sum / distancesSum(obs)
        }
    }
  }

  /**
    * computes the (absolute) speed with a lower bound of
    * MinSpeedMetersPerSecond (aka 5mph, set above) and an upper bound
    * of the freeflow speed:
    * [MinSpeedMetersPerSecond, FreeFlow]
    *
    * @param e the edge to compute
    * @return the absolute speed diff
    */
  def absoluteSpeedObservation(e: EdgeBPR): Double = {
    val ff  = e.freeFlowSpeed.value
    val obs = e.observedSpeed.value
    math.max(MinSpeedMetersPerSecond, math.min(obs, ff))
  }

  /**
    * computes the (absolute) difference in speed between the free flow
    * and the observed current speed of a link, with a lower bound of
    * MinSpeedMetersPerSecond (aka 5mph, above) and an upper bound
    * of the freeflow speed:
    * [-FreeFlow, 0]
    *
    * @param e the edge to compute
    * @return the absolute speed diff
    */
  def absoluteSpeedDiff(e: EdgeBPR): Double = {
    val ff   = e.freeFlowSpeed.value
    val obs  = e.observedSpeed.value
    val diff = obs - ff
    math.max(-ff, math.min(0.0, diff))
  }

  /**
    * computes the (relative) difference in speed between the free flow
    * and the observed current speed of a link, that results in a value
    * in the range [0, 1] where 1 == free flow (100% speed)
    *
    * @param e the edge to compute
    * @return the absolute speed diff
    */
  def relativeSpeedDiff(e: EdgeBPR): Double = {
    val ff   = e.freeFlowSpeed.value
    val obs  = e.observedSpeed.value
    val diff = obs / ff
    math.max(0.0, math.min(1.0, diff))
  }

  /**
    * apply an observation function to an edge and then weight the result
    * by the edge distance
    *
    * @param obsFn function to generate an observation
    * @param e edge to observe
    * @return the distance-weighted observation
    */
  def distanceWeightedObservation(obsFn: EdgeBPR => Double)(e: EdgeBPR): Double = {
    val obs         = obsFn(e)
    val weightedObs = obs * e.distance.value
    weightedObs
  }

  def distancesSum(edges: List[EdgeBPR]): Double = edges.map(_.distance.value).sum
}

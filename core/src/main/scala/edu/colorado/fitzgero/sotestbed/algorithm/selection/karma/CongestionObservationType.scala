package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

sealed trait CongestionObservationType

object CongestionObservationType {

  /**
    * observes only the travel time increase at the request link location
    * when calculating the observed congestion effect
    */
//  case object Disaggregate extends CongestionObservation

  /**
    * takes the mean congestion effect across a batch of requests
    */
  case object MeanFromBatch extends CongestionObservationType

  /**
    * takes the max congestion effect across a batch of requests
    */
  case object MaxFromBatch extends CongestionObservationType

  /**
    * combines an observation accumulation method combined with a transform on the output
    * @param observation observation type
    * @param transform transform to apply
    */
  case class WithTransform(observation: CongestionObservationType, transform: CongestionTransformType)
      extends CongestionObservationType

  // ..others? weighted mean, by volume? or, expand the sample beyond the
  // request batch link locations to a broader neighborhood? or,
  // get the in-links/out-links for each?

  final case class CongestionObservationResult(
    observationType: CongestionObservationType,
    freeFlowValues: List[Double],
    linkCounts: List[Double],
    observedValues: List[Double],
    freeFlowAccumulated: Double,
    observedAccumulated: Double,
    linkCountsAccumulated: Double,
    increaseAccumulated: Double
  )

  final case class AccumulatedObservation(
    freeFlowAccumulated: Double,
    observedAccumulated: Double,
    agentCounts: Double,
    increaseAccumulated: Double
  )

  implicit class CongestionObservationExtensionMethods(c: CongestionObservationType) {

    def accumulateObservations(
      freeFlowTravelTimes: List[Double],
      observedTravelTimes: List[Double],
      agentCounts: List[Double],
      n: Int
    ): AccumulatedObservation = c match {
      case MeanFromBatch =>
        val ffAcc         = freeFlowTravelTimes.sum / n
        val observedAcc   = observedTravelTimes.sum / n
        val linkCountsAcc = agentCounts.sum / n
        val increaseAcc   = (observedAcc - ffAcc) / ffAcc
        AccumulatedObservation(ffAcc, observedAcc, linkCountsAcc, increaseAcc)
      case MaxFromBatch =>
        val ffAcc         = freeFlowTravelTimes.max
        val observedAcc   = observedTravelTimes.max
        val linkCountsAcc = agentCounts.max
        val increaseAcc   = (observedAcc - ffAcc) / ffAcc
        AccumulatedObservation(ffAcc, observedAcc, linkCountsAcc, increaseAcc)
      case WithTransform(observation, transform) =>
        val obs = observation.accumulateObservations(freeFlowTravelTimes, observedTravelTimes, agentCounts, n)
        transform.applyTransform(obs)
    }

    /**
      * observes the network conditions at the locations listed
      * @param roadNetwork the network state
      * @param marginalCostFunction marginal cost/flow function
      * @param edges locations in network to observe
      * @return network conditions observations, or None if
      */
    def observeCongestion(
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      marginalCostFunction: EdgeBPR => Flow => Cost,
      edges: List[EdgeId]
    ): IO[Option[CongestionObservationResult]] = {

      val edgeDataF =
        edges.traverse { edgeId =>
          roadNetwork.edge(edgeId).flatMap {
            case None                => IO.raiseError(new Error(s"missing edge $edgeId"))
            case Some(edgeIdAndAttr) => IO.pure(edgeIdAndAttr)
          }
        }
      edgeDataF.map { edgeData =>
        val observations = for {
          edgeIdAndAttr <- edgeData
        } yield {
          val observed   = marginalCostFunction(edgeIdAndAttr.attribute)(Flow.Zero)
          val agentCount = edgeIdAndAttr.attribute.flow.value
          val ff         = edgeIdAndAttr.attribute.freeFlowCost
          (agentCount, ff.value, observed.value)
        }

        if (observations.isEmpty) None
        else {
          val n                                       = observations.length
          val (agentCounts, freeFlowTTs, observedTTs) = observations.unzip3
          val acc                                     = c.accumulateObservations(freeFlowTTs, observedTTs, agentCounts, n)

          val result = CongestionObservationResult(
            observationType = MeanFromBatch,
            freeFlowValues = freeFlowTTs,
            linkCounts = agentCounts,
            observedValues = observedTTs,
            freeFlowAccumulated = acc.freeFlowAccumulated,
            observedAccumulated = acc.observedAccumulated,
            linkCountsAccumulated = acc.agentCounts,
            increaseAccumulated = acc.increaseAccumulated
          )
          Some(result)
        }

      }
    }
  }

}

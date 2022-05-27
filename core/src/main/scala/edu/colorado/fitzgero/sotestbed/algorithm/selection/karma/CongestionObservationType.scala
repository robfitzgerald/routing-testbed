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

  // ..others? weighted mean, by volume? or, expand the sample beyond the
  // request batch link locations to a broader neighborhood? or,
  // get the in-links/out-links for each?

  case class CongestionObservationResult(
    observationType: CongestionObservationType,
    freeFlowValues: List[Double],
    linkCounts: List[Double],
    observedValues: List[Double],
    freeFlowAccumulated: Double,
    observedAccumulated: Double,
    linkCountsAccumulated: Double,
    increaseAccumulated: Double
  )

  implicit class CongestionObservationExtensionMethods(c: CongestionObservationType) {

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
          val observed  = marginalCostFunction(edgeIdAndAttr.attribute)(Flow.Zero)
          val linkCount = edgeIdAndAttr.attribute.flow.value
          val ff        = edgeIdAndAttr.attribute.freeFlowCost
          (linkCount, ff.value, observed.value)
        }

        if (observations.isEmpty) None
        else {
          val n                                      = observations.length
          val (linkCounts, freeFlowTTs, observedTTs) = observations.unzip3
          val (ffAcc, observedAcc, linkCountsAcc, increaseAcc) = c match {
            case MeanFromBatch =>
              val ffAcc         = freeFlowTTs.sum / n
              val observedAcc   = observedTTs.sum / n
              val linkCountsAcc = linkCounts.sum / n
              val increaseAcc   = (observedAcc - ffAcc) / ffAcc
              (ffAcc, observedAcc, linkCountsAcc, increaseAcc)
            case MaxFromBatch =>
              val ffAcc         = freeFlowTTs.max
              val observedAcc   = observedTTs.max
              val linkCountsAcc = linkCounts.max
              val increaseAcc   = (observedAcc - ffAcc) / ffAcc
              (ffAcc, observedAcc, linkCountsAcc, increaseAcc)
          }

          val result = CongestionObservationResult(
            observationType = MeanFromBatch,
            freeFlowValues = freeFlowTTs,
            linkCounts = linkCounts,
            observedValues = observedTTs,
            freeFlowAccumulated = ffAcc,
            observedAccumulated = observedAcc,
            linkCountsAccumulated = linkCountsAcc,
            increaseAccumulated = increaseAcc
          )
          Some(result)
        }

      }
    }
  }

}

package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

sealed trait CongestionObservationType

object CongestionObservationType {

  /**
    * samples mean congestion increase across the entire road network
    */
  case object MeanFromNetwork extends CongestionObservationType

  /**
    * samples max congestion increase across the entire road network
    */
  case object MaxFromNetwork extends CongestionObservationType

  /**
    * takes the mean congestion increase across a batch of requests
    */
  case object MeanFromBatch extends CongestionObservationType

  /**
    * takes the max congestion increase across a batch of requests
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

  final case class LinkObservation(
    flowCounts: Double = 0.0,
    freeFlowTravelTime: Double = 0.0,
    observedTravelTime: Double = 0.0
  ) {
    val increase: Double = (this.observedTravelTime - this.freeFlowTravelTime) / this.freeFlowTravelTime

    def +(that: LinkObservation): LinkObservation = this.copy(
      flowCounts = this.flowCounts + that.flowCounts,
      freeFlowTravelTime = this.freeFlowTravelTime + that.freeFlowTravelTime,
      observedTravelTime = this.observedTravelTime + that.observedTravelTime
    )

    def /(n: Int): LinkObservation = this.copy(
      flowCounts = this.flowCounts / n,
      freeFlowTravelTime = this.freeFlowTravelTime / n,
      observedTravelTime = this.observedTravelTime / n
    )
  }

  object LinkObservation {
    def empty: LinkObservation = LinkObservation()
  }

  final case class CongestionObservationResult(
    observationType: CongestionObservationType,
    freeFlowAccumulated: Double,
    observedAccumulated: Double,
    linkCountsAccumulated: Double,
    increaseAccumulated: Double
  )

  implicit class CongestionObservationExtensionMethods(c: CongestionObservationType) {

    def selectEdges(
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      agentEdges: List[EdgeId]
    ): List[EdgeId] = c match {
      case MeanFromNetwork               => roadNetwork.edgeIds
      case MaxFromNetwork                => roadNetwork.edgeIds
      case MeanFromBatch                 => agentEdges
      case MaxFromBatch                  => agentEdges
      case WithTransform(observation, _) => observation.selectEdges(roadNetwork, agentEdges)
    }

    def sampleNetwork(
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      marginalCostFunction: EdgeBPR => Flow => Cost,
      edgesToSample: List[EdgeId]
    ): IO[List[LinkObservation]] = {

      val edgeDataF =
        edgesToSample.traverse { edgeId =>
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
          LinkObservation(agentCount, ff.value, observed.value)
        }
        observations
      }
    }

    def accumulate(obs: List[LinkObservation]): Option[LinkObservation] =
      if (obs.isEmpty) None
      else {
        val acc = c match {
          case MeanFromNetwork =>
            val meanObs = obs.foldLeft(LinkObservation.empty) { _ + _ } / obs.length
            Some(meanObs)
          case MaxFromNetwork =>
            val maxObs = obs.maxBy(_.increase)
            Some(maxObs)
          case MeanFromBatch =>
            val meanObs = obs.foldLeft(LinkObservation.empty) { _ + _ } / obs.length
            Some(meanObs)
          case MaxFromBatch =>
            val maxObs = obs.maxBy(_.increase)
            Some(maxObs)
          case WithTransform(observation, _) =>
            observation.accumulate(obs)
        }
        acc
      }

    /**
      * observes the network conditions at the locations listed
      * @param roadNetwork the network state
      * @param marginalCostFunction marginal cost/flow function
      * @param agentEdges locations in network where driver agents are
      * @return network conditions observations, or None if
      */
    def observeCongestion(
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      marginalCostFunction: EdgeBPR => Flow => Cost,
      agentEdges: List[EdgeId]
    ): IO[Option[CongestionObservationResult]] = {
      val edges = c.selectEdges(roadNetwork, agentEdges)
      val result = for {
        sample <- c.sampleNetwork(roadNetwork, marginalCostFunction, edges)
        accumulated = c.accumulate(sample)
      } yield {
        accumulated.map { acc =>
          CongestionObservationResult(
            observationType = c,
            freeFlowAccumulated = acc.freeFlowTravelTime,
            observedAccumulated = acc.observedTravelTime,
            linkCountsAccumulated = acc.flowCounts,
            increaseAccumulated = acc.increase
          )
        }
      }
      result
    }
  }
}

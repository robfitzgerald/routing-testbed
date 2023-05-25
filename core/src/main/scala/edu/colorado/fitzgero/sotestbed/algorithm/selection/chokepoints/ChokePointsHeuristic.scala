package edu.colorado.fitzgero.sotestbed.algorithm.selection.chokepoints

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetworkIO
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.numeric.Flow
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import scala.annotation.tailrec
import edu.colorado.fitzgero.sotestbed.algorithm.batching.EdgeData
import edu.colorado.fitzgero.sotestbed.model.numeric.NonNegativeNumber
import edu.colorado.fitzgero.sotestbed.model.agent.Response

class ChokePointsHeuristic(minimumReplanningLeadTime: SimTime, highlyCongestedThreshold: Double)
    extends SelectionAlgorithm
    with LazyLogging {

  def selectRoutes(
    batchId: String,
    alts: Map[Request, List[Path]],
    currentSimTime: SimTime,
    roadNetwork: RoadNetworkIO,
    bank: Map[String, Karma],
    pathToMarginalFlowsFunction: (RoadNetworkIO, Path) => IO[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: EdgeBPR => Flow => Cost
  ): IO[SelectionAlgorithm.SelectionAlgorithmResult] =
    throw new NotImplementedError("must call 'build' method first")

  def build(hist: ActiveAgentHistory): SelectionAlgorithm = new SelectionAlgorithm {

    import ChokePointsHeuristic._

    def selectRoutes(
      batchId: String,
      alts: Map[Request, List[Path]],
      currentSimTime: SimTime,
      roadNetwork: RoadNetworkIO,
      bank: Map[String, Karma],
      pathToMarginalFlowsFunction: (RoadNetworkIO, Path) => IO[List[(EdgeId, Flow)]],
      combineFlowsFunction: Iterable[Flow] => Flow,
      marginalCostFunction: EdgeBPR => Flow => Cost
    ): IO[SelectionAlgorithm.SelectionAlgorithmResult] = {
      val withFutureEdges = alts.toList.traverse {
        case (req, reqAlts) =>
          for {
            agentData <- hist.getNewestDataOrError(req.agent)
            edges = edgesSuitableForReplanning(agentData.remainingRoute, minimumReplanningLeadTime)
          } yield (req, reqAlts.head, edges)
      }

      val replanned = IO.fromEither(withFutureEdges).flatMap {
        _.traverse {
          case (req, path, edges) =>
            for {
              edgesW <- edges.traverse(e => congestion(e, roadNetwork).map(c => (e, c)))
            } yield {
              edgesW.filter { case (e, c) => c < highlyCongestedThreshold } match {
                case Nil => None
                case nonEmptyCongestedLinks =>
                  val congStr = nonEmptyCongestedLinks
                    .map { case (e, c) => f"${e.edgeId}:${c * 100}%.2f%%" }
                    .mkString("[", ",", "]")
                  val len = nonEmptyCongestedLinks.length
                  logger.info(f"request ${req.agent} has highly-congested links: $congStr, applying replanning route")
                  Some(req, path)
              }
            }
        }
      }

      val result = replanned.map { result =>
        // construct the responses
        val responses = result.flatten.map {
          case (req, path) =>
            val edgeList = path.map(_.edgeId)
            val cost     = path.map(_.cost).foldLeft(Cost.Zero) { _ + _ }
            Response(req, 0, edgeList, cost)
        }

        // some stuff for logging
        // val selfishCost = costsUo.overallCost
        // val optimalCost = costsSo.overallCost
        val avgAlts: Double =
          if (alts.isEmpty) 0d else alts.map { case (_, alts) => alts.size }.sum.toDouble / alts.size
        // val travelTimeDiff: Cost     = optimalCost - selfishCost
        // val meanTravelTimeDiff: Cost = Cost((optimalCost - selfishCost).value / alts.size)
        // val (assignedUo, assignedSo) = selections.partition { case (_, idx, _) => idx == 0 }
        // val dropped                  = selections.length - filteredSelections.length
        logger.info(f"BATCH $batchId")
        // logger.info(f"ROUTES - ${assignedUo.length} UO | ${assignedSo.length} SO")
        // logger.info(f"AGENTS: ${responses.length} AVG_ALTS: $avgAlts%.2f SAMPLES: 1")
        // logger.info(
        //   f"COST_EST: BEST $optimalCost, SELFISH $selfishCost, " +
        //     f"DIFF ${travelTimeDiff.value}%.2f AVG_DIFF ${meanTravelTimeDiff.value}%.2f"
        // )

        // responses and analytics
        SelectionAlgorithm.SelectionAlgorithmResult(
          selectedRoutes = responses,
          estimatedCost = Cost.Zero,         // optimalCost,
          selfishCost = Cost.Zero,           // selfishCost,
          travelTimeDiff = Cost.Zero,        //travelTimeDiff,
          averageTravelTimeDiff = Cost.Zero, //meanTravelTimeDiff,
          samples = NonNegativeNumber.One,
          updatedBank = bank,
          ratioOfSearchSpaceExplored = 0.0
        )
      }

      result
    }
  }
}

object ChokePointsHeuristic {

  /**
    * finds edges in the future of a trip that exceed some lead time. if any edges are missing
    * travel time estimates, an empty list is returned.
    *
    * @param edges
    * @param leadTime
    * @return
    */
  @tailrec
  def edgesSuitableForReplanning(edges: List[EdgeData], leadTime: SimTime): List[EdgeData] = {
    edges match {
      case Nil => Nil
      case edge :: remaining =>
        edge.estimatedTimeAtEdge match {
          case None => Nil
          case Some(timeEstimate) =>
            val updatedLead = SimTime(math.max(0, (leadTime - timeEstimate).value))
            if (updatedLead == SimTime.Zero) remaining
            else edgesSuitableForReplanning(remaining, updatedLead)
        }
    }
  }

  def congestion(edge: EdgeData, rn: RoadNetworkIO): IO[Double] = {
    for {
      eaOpt <- rn.edge(edge.edgeId)
      ea    <- IO.fromOption(eaOpt)(new Error(s"edge ${edge.edgeId} missing attr"))
      ttEst <- IO.fromOption(edge.estimatedTimeAtEdge)(new Error(s"internal error"))
    } yield {
      val fftt     = ea.attribute.freeFlowTravelTime.value
      val dist     = ea.attribute.distance.value
      val speedEst = dist / ttEst.value.toDouble
      val ffSpeed  = dist / fftt
      ffSpeed / speedEst
    }
  }
}

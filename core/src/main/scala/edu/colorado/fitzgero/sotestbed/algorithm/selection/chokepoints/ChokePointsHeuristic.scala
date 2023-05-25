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

class ChokePointsHeuristic(
  minimumReplanningLeadTime: SimTime,
  maximumReplanningLeadTime: SimTime,
  highlyCongestedThreshold: Double
) extends SelectionAlgorithm
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
            edges = edgesSuitableForReplanning(
              agentData.remainingRoute,
              minimumReplanningLeadTime,
              maximumReplanningLeadTime
            )
          } yield (req, reqAlts.head, edges)
      }

      val replanned = IO.fromEither(withFutureEdges).flatMap {
        _.traverse {
          case (req, path, edges) =>
            for {
              edgesW <- edges.traverse(e => congestion(e, roadNetwork).map(c => (e, c)))
            } yield {
              edgesW.filter { case (e, c) => c < highlyCongestedThreshold } match {
                case Nil                                             => None
                case congestedLinks if overlap(congestedLinks, path) => None // don't replan on those same links
                case congestedLinks =>
                  val congStr = congestedLinks
                    .map { case (e, c) => f"${e.edgeId}:${c * 100}%.2f%%" }
                    .mkString("[", ",", "]")
                  val len = congestedLinks.length
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

  def apply(
    minReplanningLeadTime: SimTime,
    maxReplanningLeadTime: SimTime,
    highlyCongestedThreshold: Double
  ): ChokePointsHeuristic = {
    val validMinTime  = minReplanningLeadTime >= SimTime.Zero
    val validMaxTime  = maxReplanningLeadTime >= SimTime.Zero
    val validTimeDiff = minReplanningLeadTime < maxReplanningLeadTime
    val validThresh   = 0.0 <= highlyCongestedThreshold && highlyCongestedThreshold <= 1.0
    assert(validMinTime, s"minimum replanning lead time $minReplanningLeadTime must be non-negative")
    assert(validMaxTime, s"maximum replanning lead time $maxReplanningLeadTime must be non-negative")
    assert(
      validTimeDiff,
      s"min replanning lead time $minReplanningLeadTime must be less than max $maxReplanningLeadTime"
    )
    assert(validThresh, s"highly congested threshold $highlyCongestedThreshold must be in range [0, 1]")
    new ChokePointsHeuristic(minReplanningLeadTime, maxReplanningLeadTime, highlyCongestedThreshold)
  }

  /**
    * finds edges in the future of a trip that exceed some lead time. if any edges are missing
    * travel time estimates, we stop the search prematurely.
    *
    * travel times come from estimates that are impacted by current network conditions.
    *
    * @param edges
    * @param minLeadTime time that must be exhausted before we start accumulating edges
    * @param maxLeadTime after this time, we no longer accumulate edges
    * @return
    */
  def edgesSuitableForReplanning(edges: List[EdgeData], minLeadTime: SimTime, maxLeadTime: SimTime): List[EdgeData] = {
    @tailrec
    def _search(
      remaining: List[EdgeData],
      min: SimTime,
      max: SimTime,
      found: List[EdgeData] = List.empty
    ): List[EdgeData] = {
      remaining match {
        case Nil => found
        case edge :: remaining =>
          edge.estimatedTimeAtEdge match {
            case None => found
            case Some(edgeTime) =>
              val nextMin = SimTime(math.max(0, (min - edgeTime).value))
              val nextMax = SimTime(math.max(0, (max - edgeTime).value))
              if (nextMax == SimTime.Zero) found
              else if (minLeadTime > SimTime.Zero) _search(remaining, nextMin, nextMax, found)
              else _search(remaining, nextMin, nextMax, edge +: found)
          }
      }
    }
    _search(edges, minLeadTime, maxLeadTime)
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

  def overlap(congestedEdges: List[(EdgeData, Double)], replanningPath: Path): Boolean =
    congestedEdges
      .map { case (e, _) => e.edgeId }
      .toSet
      .intersect(replanningPath.map { _.edgeId }.toSet)
      .nonEmpty
}

package edu.colorado.fitzgero.sotestbed.matsim.app

import java.nio.file.Path

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.algorithm.search.DijkstraSearch
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, TraverseDirection}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPRCostOps
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork

object MonteCarloTravelTimeLowerBoundsOps {

  def monteCarloTravelTimeLowerBoundSample(network: Path, seed: Long, n: Int): Either[String, Double] = {
    for {
      roadNetwork <- LocalAdjacencyListFlowNetwork.fromMATSimXML(network.toFile)
      dijkstrasSearch = DijkstraSearch.edgeOrientedShortestPath(roadNetwork, EdgeBPRCostOps.freeFlowCostFunction) _
      edgeIds         = roadNetwork.edges.keys.toArray
    } yield {
      val random: Random = new Random(seed)
      val sumOfTravelTimes: Double = (1 to n).foldLeft(0.0) { (acc, i) =>
        if (i % 10000 == 0) {
          val currentAvg: Double = acc / i
          println(f"running ${i}th sample with current average $currentAvg%.2f")
        }
        val src: EdgeId = edgeIds(random.nextInt(edgeIds.length))
        val dst: EdgeId = edgeIds(random.nextInt(edgeIds.length))
        dijkstrasSearch(src, dst, TraverseDirection.Forward).unsafeRunSync() match {
          case None =>
            acc
          case Some(path) =>
            val travelTime: Double = if (path.isEmpty) 0.0 else path.map { _.cost.value }.sum
            acc + travelTime
        }
      }
      val travelTimeEstimate = sumOfTravelTimes / n.toDouble
      travelTimeEstimate
    }
  }
}

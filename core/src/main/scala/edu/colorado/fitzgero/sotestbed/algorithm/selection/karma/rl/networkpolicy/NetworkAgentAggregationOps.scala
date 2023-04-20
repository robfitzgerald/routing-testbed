package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import cats.effect.IO
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR

object NetworkAgentAggregationOps {

  def applyAggregation(
    zoneEdges: List[EdgeId],
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    agg: ObservationAggregation
  ): IO[Double] = {
    roadNetwork.edges(zoneEdges).map { eas =>
      val edges       = eas.map { _.attribute }
      val observation = agg.aggregate(edges)
      observation
    }
  }
}

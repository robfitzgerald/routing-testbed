package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import cats.effect.IO
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, NonNegativeNumber, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork, VertexId}

abstract class KSPAlgorithm {

  def terminationFunction: KSPAlgorithm.AltPathsState => Boolean

  def generateAlts(
    requests: List[Request],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    costFunction: EdgeBPR => Cost
  ): IO[KSPAlgorithm.AltPathsResult]
}

object KSPAlgorithm {

  final case class AltPathsResult(
    alternatives: Map[Request, List[Path]],
    runtime: RunTime = RunTime.Zero
  )

  final case class VertexWithDistance(vertexId: VertexId, cost: Cost)

  final case class AltPathsState(
    intersectionVertices: List[VertexWithDistance],
    startTime: Long,
    pathsSeen: NonNegativeNumber = NonNegativeNumber.Zero,
    alts: List[(Path, Cost)] = List.empty
  )
}

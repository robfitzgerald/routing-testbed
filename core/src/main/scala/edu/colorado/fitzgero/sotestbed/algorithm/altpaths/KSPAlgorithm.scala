package edu.colorado.fitzgero.sotestbed.algorithm.altpaths
import cats.{Monad, Parallel}

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, NaturalNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork, VertexId}

abstract class KSPAlgorithm[F[_]: Monad, V, E] {

  def terminationFunction: KSPAlgorithm.AltPathsState => Boolean

  def generateAlts(requests: List[Request],
                   roadNetwork: RoadNetwork[F, V, E],
                   costFunction: E => Cost): F[KSPAlgorithm.AltPathsResult]
}

object KSPAlgorithm {

  final case class AltPathsResult(
    alternatives: Map[Request, List[Path]]
  )

  final case class VertexWithDistance(vertexId: VertexId, cost: Cost)

  final case class AltPathsState(
    intersectionVertices: List[VertexWithDistance],
    startTime: Long,
    pathsSeen: NaturalNumber = NaturalNumber.Zero,
    alts: List[(Path, Cost)] = List.empty,
  )
}

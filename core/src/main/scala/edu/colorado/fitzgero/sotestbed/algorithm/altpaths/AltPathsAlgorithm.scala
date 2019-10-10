package edu.colorado.fitzgero.sotestbed.algorithm.altpaths
import cats.{Monad, Parallel}

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, NaturalNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork, VertexId}

abstract class AltPathsAlgorithm[F[_]: Monad : Parallel, V, E] {

  def generateAlts(requests: List[Request],
                   roadNetwork: RoadNetwork[F, V, E],
                   costFunction: E => Cost,
                   terminationFunction: AltPathsAlgorithm.AltPathsState => Boolean)
    : F[AltPathsAlgorithm.AltPathsResult]
}

object AltPathsAlgorithm {

  final case class AltPathsResult(
      alternatives: Map[Request, List[Path]]
  )

  final case class VertexWithDistance(vertexId: VertexId, cost: Cost)

  final case class AltPathsState(
      intersectionVertices: List[VertexWithDistance],
      alts: List[(Path, Cost)] = List.empty,
      pathsSeen: NaturalNumber = NaturalNumber.Zero,
  )
}

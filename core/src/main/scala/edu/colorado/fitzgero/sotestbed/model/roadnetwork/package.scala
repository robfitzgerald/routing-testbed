package edu.colorado.fitzgero.sotestbed.model

import cats.effect.IO
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR

package object roadnetwork {

  type Path = List[PathSegment]
  val EmptyPath: Path = List.empty[PathSegment]

  object Path {
    def mkString(path: Path): String = path.map { _.edgeId }.mkString("[", "->", "]")
  }

  // a lot of this project was designed to sit atop a road network
  // abstraction but also to try and capture the simulation state
  // as a value. after a few years of work, it has become somewhat
  // deprecated and most implementations do not code to this abstraction,
  // instead expecting a RoadNetwork which uses the Coordinate type for
  // vertices, EdgeBPR type for edges, and captures effects via IO.
  type RoadNetworkIO = RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]
}

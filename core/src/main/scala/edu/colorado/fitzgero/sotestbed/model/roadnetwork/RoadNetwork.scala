package edu.colorado.fitzgero.sotestbed.model.roadnetwork

trait RoadNetwork [F[_], V, E] extends
  RoadNetworkState[V, E] with
  RoadNetworkMutable[F, E] {}

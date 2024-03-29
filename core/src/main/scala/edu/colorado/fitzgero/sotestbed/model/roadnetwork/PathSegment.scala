package edu.colorado.fitzgero.sotestbed.model.roadnetwork

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import cats.effect.IO
import edu.colorado.fitzgero.sotestbed.algorithm.batching.EdgeData
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

case class PathSegment(edgeId: EdgeId, cost: Cost) {
  override def toString: String = s"PathSegment($edgeId, $cost)"

  def toEdgeDataButRetainCost(rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]): IO[EdgeData] =
    PathSegment.toEdgeData(this, rn)

  def updateCostEstimate(rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]): IO[PathSegment] =
    PathSegment.updateCostEstimate(this, rn)

  def toEdgeDataWithUpdatedCost(rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]): IO[EdgeData] =
    for {
      uc <- PathSegment.updateCostEstimate(this, rn)
      ed <- PathSegment.toEdgeData(uc, rn)
    } yield ed
}

object PathSegment {

  def updateCostEstimate(
    pathSegment: PathSegment,
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]
  ): IO[PathSegment] =
    rn.edge(pathSegment.edgeId)
      .flatMap {
        case None => IO.raiseError(new Error(s"road network missing edge ${pathSegment.edgeId}"))
        case Some(ea) =>
          val updatedCost = Cost(ea.attribute.observedTravelTime.value)
          val updatedSeg  = pathSegment.copy(cost = updatedCost)
          IO.pure(updatedSeg)
      }

  /**
    * helper to deal with the friction between these competing definitions
    * of a link segment and it's attributes
    *
    * @param pathSegment
    * @param rn
    * @return
    */
  def toEdgeData(
    pathSegment: PathSegment,
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]
  ): IO[EdgeData] = {
    val edgeId = pathSegment.edgeId
    val time   = SimTime(pathSegment.cost.value)
    val err    = s"invalid vertices for edge $edgeId"
    for {
      srcId <- rn.source(edgeId).flatMap(IO.fromOption(_)(new Error(err)))
      dstId <- rn.destination(edgeId).flatMap(IO.fromOption(_)(new Error(err)))
      edge  <- rn.edge(edgeId).flatMap(IO.fromOption(_)(new Error(err)))
      src   <- rn.vertex(srcId).flatMap(IO.fromOption(_)(new Error(err)))
      dst   <- rn.vertex(dstId).flatMap(IO.fromOption(_)(new Error(err)))
    } yield EdgeData(edgeId, src.attribute, dst.attribute, edge.attribute.distance.value, Some(time))
  }
}

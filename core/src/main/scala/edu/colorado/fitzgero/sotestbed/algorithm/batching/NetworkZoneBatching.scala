package edu.colorado.fitzgero.sotestbed.algorithm.batching

import cats.implicits._
import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.Batching.{BatchingInstruction, BatchingStrategy}
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.algorithm.batching.BatchingFunction
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy.NetworkZone
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.algorithm.grid.CoordinateGrid2
import com.typesafe.scalalogging.LazyLogging

final case class NetworkZoneBatching[T](
  extractIndex: RoadNetwork[IO, Coordinate, EdgeBPR] => RouteRequestData => IO[(T, RouteRequestData)],
  batchIdLookup: T => Option[String],
  zones: Map[String, List[EdgeId]]
) extends BatchingFunction
    with LazyLogging {

  /**
    * assigns incoming requests to batches based on their "zone".
    * requests are grouped using the batchIdLookup which assigns requests
    * to BatchIds by their current EdgeId. these requests are in the same
    * "zone". if an EdgeId does not appear in the lookup, the request is
    * dropped (it's not in a "zone").
    *
    * @param roadNetwork the current road network state
    * @param activeRouteRequests agents which are available for SO routing requests
    * @param currentTime the current sim time
    * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
    */
  def updateBatchingStrategy(
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    activeRouteRequests: List[RouteRequestData],
    currentTime: SimTime
  ): IO[Option[BatchingFunction.BatchingResult]] = {

    // find, for each request, which index / batch identifier it is associated with
    val extractFnInstance = extractIndex(roadNetwork)
    val requestsByIndex   = activeRouteRequests.traverse { extractFnInstance }

    val requestsByBatch = requestsByIndex.map {
      _.flatMap {
        case (t, routeRequestData) =>
          batchIdLookup(t).map { batchId => (batchId, routeRequestData.request) }
      }
    }

    requestsByBatch.flatMap {
      case Nil => IO.pure(None)
      case rbb =>
        val batches = rbb
          .groupBy { case (b, _) => b }
          .map {
            case (batchId, labeledRequests) =>
              val (_, requests) = labeledRequests.unzip
              batchId -> requests
          }
          .toList
        val result = BatchingFunction.BatchingResult(batches, zones)
        IO.pure(Some(result))
    }
  }
}

object NetworkZoneBatching {

  /**
    * use a coordinate grid to build network zones
    *
    * @param grid the coordinate grid
    * @return a NetworkZoneBatching instance that indexes BatchIds via coordinate
    * lookup in the CoordinateGrid2 instance
    */
  def apply(grid: CoordinateGrid2): NetworkZoneBatching[(Double, Double)] = {
    val lookupFn = (coord: (Double, Double)) => {
      val (x, y) = coord
      grid.getGridId(x, y).toOption
    }
    NetworkZoneBatching(extractDstVertexCoord, lookupFn, grid.edgeLookup)
  }

  /**
    * use a list of NetworkZones that indexes BatchIds by the current
    * RouteRequestData location EdgeId
    *
    * @param zones the list of zones, each with it's own BatchId and
    * list of EdgeIds in that zone
    * @return a NetworkZoneBatching instance that indexes BatchIds by
    * EdgeIds
    */
  def apply(zones: List[NetworkZone]): NetworkZoneBatching[EdgeId] = {

    val idxFn = (_: RoadNetwork[IO, Coordinate, EdgeBPR]) =>
      (rrd: RouteRequestData) => IO.pure((rrd.request.location, rrd))

    val lookupData = zones.flatMap { z => z.edges.map { e => (e, z) } }.toMap
    val lookupFn   = (edgeId: EdgeId) => lookupData.get(edgeId).map { _.batchId }
    val edgeLookup = zones.map { z => z.batchId -> z.edges }.toMap

    NetworkZoneBatching(idxFn, lookupFn, edgeLookup)
  }

  /**
    * helper used to treat coordinate pairs as an index into CoordinateGrid2 for
    * NetworkZoneBatching over coordinate grids.
    *
    * @param roadNetwork current road network state
    * @param rrd route request data
    * @return the route request data along with its coordinate as an index value
    */
  def extractDstVertexCoord(
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR]
  )(rrd: RouteRequestData): IO[((Double, Double), RouteRequestData)] = {
    for {
      edgeDstVertexIdOpt <- roadNetwork.destination(rrd.request.location)
      edgeDstVertexId <- IO.fromOption(edgeDstVertexIdOpt)(
        new Error(s"missing destination vertex id for edge ${rrd.request.location}")
      )
      vertexAndAttrOpt <- roadNetwork.vertex(edgeDstVertexId)
      vertexAndAttr    <- IO.fromOption(vertexAndAttrOpt)(new Error(s"missing vertex $edgeDstVertexId"))
    } yield {
      val idx = (vertexAndAttr.attribute.x, vertexAndAttr.attribute.y)
      (idx, rrd)
    }
  }
}

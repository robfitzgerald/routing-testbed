package edu.colorado.fitzgero.sotestbed.algorithm.batching

import cats.effect.SyncIO

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData.EdgeData
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

object BatchingOps {

  def defaultStartCoordinate(path: List[EdgeData]): Option[Coordinate] = path.headOption.map { _.linkSourceCoordinate }

//  /**
//    * traverses a path of EdgeData to find a point in the future for the agent, and
//    * returns the source coordinate of that path edge
//    *
//    * @param path a path from a request containing EdgeData
//    * @param currentTime the current SimTime
//    * @param batchPathTimeDelay a duration to append to the current time to use for search
//    * @return
//    */
//  def findCoordinateInFuture(
//    path: List[EdgeData],
//    currentTime: SimTime,
//    batchPathTimeDelay: SimTime,
//    roadNetwork: RoadNetwork[SyncIO, Coordinate, EdgeBPR]
//  ): Option[Coordinate] = {
//    if (batchPathTimeDelay == SimTime.Zero) {
//      // no traversal
//      path.headOption.map { _.linkSourceCoordinate }
//    } else {
//      // find point in future
//      val timeInFuture: SimTime = currentTime + batchPathTimeDelay
//
//      val pointInFuture: Option[Coordinate] = path
//        .takeWhile { _.estimatedTimeAtEdge < timeInFuture }
//        .lastOption
//        .map { _.linkSourceCoordinate }
//      pointInFuture
//    }
//  }
}

package edu.colorado.fitzgero.sotestbed.algorithm.batching

import scala.util.Try
import scala.util.matching.Regex

import cats.Monad

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.EdgeData
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

class GreedyCoordinateGridBatching(
  batchWindow: SimTime,
  minimumReplanningWaitTime: SimTime,
  maxBatchSize: Int,
  minX: Double,
  maxX: Double,
  minY: Double,
  maxY: Double,
  splitFactor: Int,
  batchPathTimeDelay: SimTime
) extends BatchingFunction with LazyLogging {

  // split out the coordinate space into splitFactor * splitFactor grids
  val grid: GreedyCoordinateGridBatching.CoordinateGrid =
    new batching.GreedyCoordinateGridBatching.CoordinateGrid(minX, maxX, minY, maxY, splitFactor)

  /**
    * takes the current batching strategy and any updates about replan-able agents, and spits out an
    * update to that batching strategy
    *
    * @param roadNetwork          the current road network state
    * @param currentBatchStrategy the current strategy
    * @param newBatchData         some new data about agents eligible for replanning from the system
    * @param currentTime          the current sim time
    * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
    */
  def updateBatchingStrategy[F[_]: Monad, V, E](roadNetwork: RoadNetwork[F, V, E],
                                                currentBatchStrategy: Map[SimTime, List[List[AgentBatchData]]],
                                                newBatchData: List[AgentBatchData],
                                                currentTime: SimTime): F[Option[Map[SimTime, List[List[AgentBatchData]]]]] = {

    if (newBatchData.isEmpty) {
      Monad[F].pure {
        Some {
          currentBatchStrategy
        }
      }
    } else {
      Monad[F].pure {
        val nextValidBatchTime = BatchingManager.nextValidBatchingTime(this.batchWindow, currentTime)

        // don't mess with agents whos plans are before the next valid batch time
        val (replannableOldData, fixed) = currentBatchStrategy.partition { case (time, _) => time >= nextValidBatchTime }

        // combine old agend data with new
        val merged: List[AgentBatchData] = BatchingManager.keepLatestAgentBatchData(
          oldData=replannableOldData.values.flatten.flatten.toList,
          newData=newBatchData
        )

        // find all agent's estimated location at batchPathTimeDelay time into future and find their coordinate.
        // find which grid cell the coordinate sits within, and use that cell id to group this agent for batching
        val grouped: Map[String, List[(String, AgentBatchData)]] = merged
          .flatMap { agentBatchData =>
            GreedyCoordinateGridBatching.findCoordinateInFuture(
              agentBatchData.currentEdgeRoute,
              currentTime,
              this.batchPathTimeDelay
            ) match {
              case None =>
                List.empty
              case Some(coord) =>
                List((grid.getGridId(coord.x, coord.y), agentBatchData))
            }
          }
          .groupBy { case (groupId, _) => groupId }

        val toAdd: List[List[AgentBatchData]] = grouped
          .flatMap { case (_, groupedData) =>
            groupedData
              .map { _._2 }
              .sliding(this.maxBatchSize, this.maxBatchSize)
          }
          .toList

        if (toAdd.isEmpty) {
          None
        } else {

          logger.info("SPATIAL BATCH SPLIT:\n" + grid.printGrid(grouped))

          // we can update our plan based on this grouping
          Some {
            val updatedAtTime: List[List[AgentBatchData]] = fixed.getOrElse(nextValidBatchTime, List.empty) ++ toAdd
            fixed.updated(nextValidBatchTime, updatedAtTime)
          }
        }
      }
    }
  }
}

object GreedyCoordinateGridBatching {

  /**
    * creates a simple grid from a bounding box. given an arbirary point within the box, provides a unique grouping
    * id related to the cell which contains the point, allowing for agent grouping by cell.
    * @param minX the bounding box's minimum x position
    * @param maxX the bounding box's maximum x position
    * @param minY the bounding box's minimum y position
    * @param maxY the bounding box's maximum y position
    * @param splitFactor how many cell to split the space in each dimension; i.e., splitFactor == 3 -> 9 cells
    */
  class CoordinateGrid(
    minX: Double,
    maxX: Double,
    minY: Double,
    maxY: Double,
    splitFactor: Int,
  ) {

    // step size between cells in the underlying coordinate space
    val xStep: Double = (maxX - minX) / splitFactor
    val yStep: Double = (maxY - minY) / splitFactor

    /**
      * gives cell ids which start from zero along each axis and end at splitFactor - 1
      *
      * @param x test x value
      * @param y test y value
      * @return an id, in the form "xId#yId"
      */
    def getGridId(x: Double, y: Double): String = {
      val xId = math.min(((x - minX) / xStep).toInt, splitFactor - 1)
      val yId = math.min(((y - minY) / yStep).toInt, splitFactor - 1)
      s"$xId#$yId"
    }

    /**
      * for pretty-print logging of group data
      * @param gridId stringified grid id
      * @return stringified coordinate, or nothing if invalid
      */
    def getGridCoord(gridId: String): Option[String] = gridId match {
      case CoordinateGrid.GridIdRegex(xStr, yStr) =>
        Try {
          val (xId, yId) = (xStr.toInt, yStr.toInt)
          if (xId >= splitFactor || yId >= splitFactor) {
            throw new IndexOutOfBoundsException()
          } else {
            val (x, y) = (minX + (xId * xStep), minY + (yId * yStep))
            f"($x%.4f,$y%.4f)"
          }
        }.toOption
      case _ => None
    }

    /**
      * Ordering used for pretty printing a grid based on the stringified grid indices
      */
    val GroupIdOrdering: Ordering[String] = Ordering.by {
      case CoordinateGrid.GridIdRegex(xStr, yStr) =>
        Try {
          val (xId, yId) = (xStr.toInt, yStr.toInt)
          -(xId + yId * splitFactor)
        }.toOption match {
          case None => Int.MaxValue
          case Some(ord) => ord
        }
      case _ => Int.MaxValue
    }

    /**
      * presents a spatial grid and grid counts as a string
      * @param grouped the current batch grouping
      * @return a pretty-printed representation of the batch grouping
      */
    def printGrid(grouped: Map[String, List[(String, AgentBatchData)]]): String = {
      grouped
        .map{ case (gridId, agents) => (gridId, agents.size) }
        .toList
        .sortBy{_._1}(GroupIdOrdering)
        .sliding(this.splitFactor, this.splitFactor)
        .map{ row =>
          row.map{ case (_, count) => count.toString.padTo(3, ' ') }.mkString("")
        }.mkString("\n")
    }
  }
  object CoordinateGrid {
    val GridIdRegex: Regex = """^(\d+)#(\d+)$""".r
  }

  /**
    * traverses a path of EdgeData to find a point in the future for the agent, and
    * returns the source coordinate of that path edge
    *
    * @param path a path from a request containing EdgeData
    * @param currentTime the current SimTime
    * @param batchPathTimeDelay a duration to append to the current time to use for search
    * @return
    */
  def findCoordinateInFuture(
    path: List[EdgeData],
    currentTime: SimTime,
    batchPathTimeDelay: SimTime
  ): Option[Coordinate] = {
    if (batchPathTimeDelay == SimTime.Zero) {
      // no traversal
      path.headOption.map { _.linkSourceCoordinate }
    } else {
      // find point in future
      val timeInFuture: SimTime = currentTime + batchPathTimeDelay
      val pointInFuture: Option[Coordinate] = path
        .takeWhile { _.estimatedTimeAtEdge < timeInFuture }
        .lastOption
        .map { _.linkSourceCoordinate }
      pointInFuture
    }
  }
}

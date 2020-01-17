package edu.colorado.fitzgero.sotestbed.algorithm.batching

import cats.Monad

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

class GreedyCoordinateGridODBatching(
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
  val grid: CoordinateGrid = new CoordinateGrid(minX, maxX, minY, maxY, splitFactor)

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
        // create a tag from this src coordinate and the agent's final destination as a coordinate
        // map the coordinates to string ids and concatenate them into a grouping tag
        val tagged: List[(String, String, String, AgentBatchData)] = {
          for {
            agentBatchData <- merged
            src <- BatchingOps.findCoordinateInFuture(
              agentBatchData.currentEdgeRoute,
              currentTime,
              this.batchPathTimeDelay
            )
            dst <- agentBatchData.currentEdgeRoute.lastOption.map{_.linkSourceCoordinate}
          } yield {
            val originTag: String = grid.getGridId(src.x, src.y)
            val destinationTag: String = grid.getGridId(dst.x, dst.y)
            (s"$originTag#$destinationTag", originTag, destinationTag, agentBatchData)
          }
        }

        val grouped: Map[String, List[(String, String, String, AgentBatchData)]] = tagged.groupBy { case (odTag, _, _, _) => odTag }

        val toAdd: List[List[AgentBatchData]] = grouped
          .flatMap { case (_, tuples) =>
            // this group may exceed our maxBatchSize, so, break them up based on a batch splitting function
            val agentBatchData: List[AgentBatchData] = tuples.map { case (_, _, _, group) => group }
            BatchSplittingFunction.splitUntilValid(agentBatchData, this.maxBatchSize)
          }
          .toList

        if (toAdd.isEmpty) {
          None
        } else {
          val prettyPrintMe: Map[String, List[(String, AgentBatchData)]] =
            grouped.map{ case (k, l) => k -> l.map{ case (_, o, _, data) => (o, data)} }
          logger.info(s"so batched agent locations at $currentTime:\n" + grid.printGrid(prettyPrintMe))
          logger.info(s"batch groupings:\n ${grouped.map{ case (_, v) => v.size }.mkString("[", ",", "]")}")

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



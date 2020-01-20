package edu.colorado.fitzgero.sotestbed.algorithm.batching

import cats.Monad

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.Batching.{BatchingInstruction, BatchingStrategy}
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

class GreedyCoordinateGridODBatching2(
  batchWindow: SimTime,
  minimumReplanningWaitTime: SimTime,
  maxBatchSize: Int,
  minX: Double,
  maxX: Double,
  minY: Double,
  maxY: Double,
  splitFactor: Int,
  batchPathTimeDelay: SimTime,
  tagType: String = "od"
) extends BatchingFunction2
    with LazyLogging {

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
                                                currentBatchStrategy: BatchingStrategy,
                                                newBatchData: List[AgentBatchData],
                                                currentTime: SimTime): F[Option[List[Batching.BatchingInstruction]]] = Monad[F].pure {

    newBatchData match {
      case Nil =>
        // no new batch data; use current strategy
        Some { currentBatchStrategy.values.toList }
      case _ =>
        val nextValidBatchTime = BatchingManager.nextValidBatchingTime(this.batchWindow, currentTime)

        // agents slated for the next valid batching time should not be touched
        val (replannableCurrentBatch, slatedForNextBatchingTime) =
          currentBatchStrategy
            .map { case (_, i) => i }
            .toList
            .partition { _.batchingTime > nextValidBatchTime }

        // find all agent's estimated location at batchPathTimeDelay time into future and find their coordinate.
        // create a tag from this src coordinate and the agent's final destination as a coordinate
        // map the coordinates to string ids and concatenate them into a grouping tag
        val tagged: List[(String, AgentBatchData)] = for {
          agentBatchData <- newBatchData ++ replannableCurrentBatch.map{_.agentBatchData}
          tagger <- GreedyCoordinateGridODBatching2.BatchTag.makeBatchTag(tagType, agentBatchData)
          tagged <- tagger match {
            case odTagger: GreedyCoordinateGridODBatching2.BatchTag.ODTag =>
              odTagger.tag(grid, currentTime, this.batchPathTimeDelay).map{ tag =>
                (tag, agentBatchData)
              }
          }
        } yield {
          tagged
        }

        // make sub-batches based on the tag groupings
        val grouped: Map[String, List[(String, AgentBatchData)]] = tagged.groupBy { case (tag, _) => tag }

        //
        val toAdd: List[BatchingInstruction] = grouped.flatMap {
          case (tag, tuples) =>
            // this group may exceed our maxBatchSize, so, break them up based on a batch splitting function
            val agentBatchData: List[AgentBatchData] = tuples.map { case (_, agentBatchData) => agentBatchData }
            for {
              (batch, batchIdx) <- BatchSplittingFunction.bySlidingWindow(agentBatchData, this.maxBatchSize).zipWithIndex
              batchId = s"$tag-$batchIdx"
              agentBatchData <- batch
            } yield {
              // at this point we have all the information we need to create a batch id
              // since a tag plus window index should be unique
              Batching.BatchingInstruction(
                agentBatchData,
                nextValidBatchTime,
                batchId
              )
            }

        }.toList

        if (toAdd.isEmpty) {
          None
        } else {
          val prettyPrintMe: Map[String, List[(String, AgentBatchData)]] =
            grouped.map { case (k, l) =>
              k -> l.map { case (_, agentBatchData) =>
                (agentBatchData.request.origin.value, agentBatchData)
              }
            }
          logger.info(s"so batched agent locations at $currentTime:\n" + grid.printGrid(prettyPrintMe))
          logger.info(s"batch groupings:\n ${grouped.map { case (_, v) => v.size }.mkString("[", ",", "]")}")

          // we can update our plan based on this grouping
          Some {
            slatedForNextBatchingTime ++ toAdd
          }
        }
    }
  }
}

object GreedyCoordinateGridODBatching2 {
  sealed trait BatchTag

  object BatchTag {

    def makeBatchTag(tagType: String, agentBatchData: AgentBatchData): Option[BatchTag] = {
      tagType.trim.toLowerCase match {
        case "od" => Some { ODTag(agentBatchData) }
        case _    => None
      }
    }

    final case class ODTag(agentBatchData: AgentBatchData) extends BatchTag {

      def tag(grid: CoordinateGrid, currentTime: SimTime, batchPathTimeDelay: SimTime): Option[String] =
        for {
          src <- BatchingOps.findCoordinateInFuture(
            agentBatchData.currentEdgeRoute,
            currentTime,
            batchPathTimeDelay
          )
          dst <- agentBatchData.currentEdgeRoute.lastOption.map { _.linkSourceCoordinate }
        } yield {
          val originTag: String      = grid.getGridId(src.x, src.y)
          val destinationTag: String = grid.getGridId(dst.x, dst.y)
          s"$originTag#$destinationTag"
        }
    }
  }
}

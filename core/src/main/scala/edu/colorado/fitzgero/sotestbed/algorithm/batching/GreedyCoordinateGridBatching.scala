package edu.colorado.fitzgero.sotestbed.algorithm.batching

import cats.Monad

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.Batching.{BatchingInstruction, BatchingStrategy}
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

class GreedyCoordinateGridBatching(
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
) extends BatchingFunction
    with LazyLogging {

  // split out the coordinate space into splitFactor * splitFactor grids
  val grid: CoordinateGrid = new CoordinateGrid(minX, maxX, minY, maxY, splitFactor)

  /**
    * takes the current batching strategy and any updates about replan-able agents, and spits out an
    * update to that batching strategy
    *
    * @param roadNetwork          the current road network state
    * @param activeRouteRequests agents which are available for SO routing requests
    * @param currentTime          the current sim time
    * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
    */
  def updateBatchingStrategy[F[_]: Monad, V, E](roadNetwork: RoadNetwork[F, V, E],
                                                activeRouteRequests: List[RouteRequestData],
                                                currentTime: SimTime): F[Option[List[List[Request]]]] = Monad[F].pure {

    activeRouteRequests match {
      case Nil =>
        // no new batch data; use current strategy
        None
      case _ =>
//        val nextValidBatchTime = BatchingManager.nextValidBatchingTime(this.batchWindow, currentTime)
//
//        // agents slated for the next valid batching time should not be touched
//        val (replannableCurrentBatch, slatedForNextBatchingTime) =
//          currentBatchStrategy
//            .map { case (_, i) => i }
//            .toList
//            .partition { _.batchingTime > nextValidBatchTime }
//
//        val replannableCurrentBatchData: List[AgentBatchData] =
//          replannableCurrentBatch.flatMap { i =>
//            agentBatchDataMap.get(i.agentId)
//          }

        // find all agent's estimated location at batchPathTimeDelay time into future and find their coordinate.
        // create a tag from this src coordinate and the agent's final destination as a coordinate
        // map the coordinates to string ids and concatenate them into a grouping tag
        val tagged: List[(String, RouteRequestData)] = for {
          agentBatchData <- activeRouteRequests
          tagger         <- GreedyCoordinateGridBatching.BatchTag.makeBatchTag(tagType, agentBatchData)
          tagged <- tagger match {
            case od: GreedyCoordinateGridBatching.BatchTag.ODTag =>
              od.tag(grid, currentTime, this.batchPathTimeDelay).map { tag =>
                (tag, agentBatchData)
              }
            case o: GreedyCoordinateGridBatching.BatchTag.OTag =>
              o.tag(grid, currentTime, this.batchPathTimeDelay).map { tag =>
                (tag, agentBatchData)
              }
            case d: GreedyCoordinateGridBatching.BatchTag.DTag =>
              d.tag(grid).map { tag =>
                (tag, agentBatchData)
              }
          }
        } yield {
          tagged
        }

        // make sub-batches based on the tag groupings
        val grouped: Map[String, List[(String, RouteRequestData)]] = tagged.groupBy { case (tag, _) => tag }

        //
        val toAdd: List[(RouteRequestData, String)] = grouped.flatMap {
          case (tag, tuples) =>
            // this group may exceed our maxBatchSize, so, break them up based on a batch splitting function
            val routeRequestDatas: List[RouteRequestData] = tuples.map { case (_, agentBatchData) => agentBatchData }
            for {
              (batch, batchIdx) <- BatchSplittingFunction.bySlidingWindow(routeRequestDatas, this.maxBatchSize).zipWithIndex
              batchId = s"$tag-$batchIdx"
              routeRequestData <- batch
            } yield {
              // at this point we have all the information we need to create a batch id
              // since a tag plus window index should be unique
              (routeRequestData, batchId)
            }

        }.toList

        if (toAdd.isEmpty) {
          None
        } else {
          val prettyPrintMe: Map[String, List[(String, AgentBatchData)]] =
            grouped.map {
              case (k, l) =>
                k -> l.map {
                  case (_, agentBatchData) =>
                    (agentBatchData.request.origin.value, agentBatchData)
                }
            }
          logger.info(s"so batched agent locations at $currentTime:\n" + grid.printGrid(prettyPrintMe))
          logger.info(s"batch groupings:\n ${grouped.map { case (_, v) => v.size }.mkString("[", ",", "]")}")

          // share our grouping with the world, let them smile upon our greatness
          Some {
            toAdd
              .groupBy{ case (_, batchId) => batchId }
              .values
              .map{_.map{ case (data, _) => data.request}}
              .toList
          }
        }
    }
  }
}

object GreedyCoordinateGridBatching {
  sealed trait BatchTag

  object BatchTag {

    def makeBatchTag(tagType: String, agentBatchData: RouteRequestData): Option[BatchTag] = {
      tagType.trim.toLowerCase match {
        case "od" => Some { ODTag(agentBatchData) }
        case "o"  => Some { OTag(agentBatchData) }
        case "d"  => Some { DTag(agentBatchData) }
        case _    => None
      }
    }

    final case class OTag(routeRequestData: RouteRequestData) extends BatchTag {

      def tag(grid: CoordinateGrid, currentTime: SimTime, batchPathTimeDelay: SimTime): Option[String] =
        for {
          src <- BatchingOps.findCoordinateInFuture(
            routeRequestData.currentEdgeRoute,
            currentTime,
            batchPathTimeDelay
          )
        } yield {
          val originTag: String = grid.getGridId(src.x, src.y)
          originTag
        }
    }
    final case class DTag(routeRequestData: RouteRequestData) extends BatchTag {

      def tag(grid: CoordinateGrid): Option[String] =
        for {
          dst <- routeRequestData.currentEdgeRoute.lastOption.map { _.linkSourceCoordinate }
        } yield {
          val destinationTag: String = grid.getGridId(dst.x, dst.y)
          destinationTag
        }
    }
    final case class ODTag(routeRequestData: RouteRequestData) extends BatchTag {

      def tag(grid: CoordinateGrid, currentTime: SimTime, batchPathTimeDelay: SimTime): Option[String] =
        for {
          src <- BatchingOps.findCoordinateInFuture(
            routeRequestData.currentEdgeRoute,
            currentTime,
            batchPathTimeDelay
          )
          dst <- routeRequestData.currentEdgeRoute.lastOption.map { _.linkSourceCoordinate }
        } yield {
          val originTag: String      = grid.getGridId(src.x, src.y)
          val destinationTag: String = grid.getGridId(dst.x, dst.y)
          s"$originTag#$destinationTag"
        }
    }
  }
}

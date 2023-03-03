package edu.colorado.fitzgero.sotestbed.algorithm.routing

import edu.colorado.fitzgero.sotestbed.algorithm.batching._
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork._
import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionRunnerResult

object RoutingOps {

  def preFilterBatches(
    batchingManager: BatchingManager,
    minBatchSize: Int,
    replanAtSameLink: Boolean,
    batches: List[(String, List[Request])]
  ): List[(String, List[Request])] = {
    val noSmallBatches = removeSmallBatches(minBatchSize, batches)
    if (replanAtSameLink) noSmallBatches
    else removeRequestsAtSameLink(batchingManager, noSmallBatches)
  }

  def findShortestPathForBatch(
    search: (EdgeId, EdgeId, TraverseDirection) => IO[Option[Path]],
    reqs: List[Request]
  ): IO[List[(Request, Path)]] =
    reqs
      .traverse { req =>
        search(req.location, req.destination, TraverseDirection.Forward)
          .map {
            case None                       => None
            case Some(path) if path.isEmpty => None
            case Some(path)                 => Some((req, path))
          }
      }
      .map { _.flatten }

  def extractSingularBatchResult(
    selectionResult: List[Option[SelectionRunnerResult]]
  ): IO[Option[SelectionRunnerResult]] =
    selectionResult match {
      case Nil        => IO.pure(None)
      case one :: Nil => IO.pure(one)
      case more       => IO.raiseError(new Error(s"expected 0 or 1 selection result, found ${more.length}"))
    }

  def removeRequestsAtSameLink(
    batchingManager: BatchingManager,
    batches: List[(String, List[Request])]
  ): List[(String, List[Request])] = {
    batches.flatMap {
      case (batchId, reqs) =>
        reqs.filter { req =>
          batchingManager.storedHistory.getPreviousReplanning(req.agent) match {
            case None => true
            case Some(prevReplanned) =>
              prevReplanned.request.location != req.location
          }
        } match {
          case Nil      => None
          case filtered => Some(batchId -> filtered)
        }
    }
  }

  def removeSmallBatches(minBatchSize: Int, batches: List[(String, List[Request])]): List[(String, List[Request])] =
    batches.filter { case (_, reqs) => reqs.lengthCompare(minBatchSize) >= 0 }
}

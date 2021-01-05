package edu.colorado.fitzgero.sotestbed.algorithm.grid

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

sealed trait BatchTagger {
  def tag(grid: CoordinateGrid, routeRequestData: RouteRequestData): Option[String]
}

object BatchTagger {

  val ValidBatchTags: Set[String] = Set("o", "d", "od", "c", "cd")

  def makeBatchTag(tagType: String): Option[BatchTagger] = {
    tagType.trim.toLowerCase match {
      case "o"  => Some { OTag }
      case "d"  => Some { DTag }
      case "od" => Some { ODTag }
      case "c"  => Some { CTag }
      case "cd" => Some { CDTag }
      case _    => None
    }
  }

  /**
    * uses the trip's origin (static) for grouping
    */
  final case object OTag extends BatchTagger {

    def tag(grid: CoordinateGrid, routeRequestData: RouteRequestData): Option[String] =
      for {
        src <- originCoordinate(routeRequestData)
      } yield {
        val originTag: String = grid.getGridId(src.x, src.y)
        originTag
      }
  }

  /**
    * uses the trip's destination (static) for grouping
    */
  final case object DTag extends BatchTagger {

    def tag(grid: CoordinateGrid, routeRequestData: RouteRequestData): Option[String] =
      for {
        dst <- destinationCoordinate(routeRequestData)
      } yield {
        val destinationTag: String = grid.getGridId(dst.x, dst.y)
        destinationTag
      }
  }

  /**
    * uses the trip's origin and destination (both static) for grouping
    */
  final case object ODTag extends BatchTagger {

    def tag(grid: CoordinateGrid, routeRequestData: RouteRequestData): Option[String] =
      for {
        src <- originCoordinate(routeRequestData)
        dst <- destinationCoordinate(routeRequestData)
      } yield {
        val originTag: String      = grid.getGridId(src.x, src.y)
        val destinationTag: String = grid.getGridId(dst.x, dst.y)
        s"$originTag#$destinationTag"
      }
  }

  /**
    * uses the trip's current location (dynamic) for grouping
    */
  final case object CTag extends BatchTagger {

    def tag(grid: CoordinateGrid, routeRequestData: RouteRequestData): Option[String] =
      for {
        src <- currentLocationCoordinate(routeRequestData)
      } yield {
        val currentTag: String = grid.getGridId(src.x, src.y)
        currentTag
      }
  }

  /**
    * uses the trip's current location (dynamic) and destination (static) for grouping
    */
  final case object CDTag extends BatchTagger {

    def tag(grid: CoordinateGrid, routeRequestData: RouteRequestData): Option[String] =
      for {
        src <- currentLocationCoordinate(routeRequestData)
        dst <- destinationCoordinate(routeRequestData)
      } yield {
        val currentTag: String     = grid.getGridId(src.x, src.y)
        val destinationTag: String = grid.getGridId(dst.x, dst.y)
        s"$currentTag#$destinationTag"
      }
  }

  def originCoordinate(routeRequestData: RouteRequestData): Option[Coordinate] =
    routeRequestData.experiencedRoute.headOption.map { _.linkDestinationCoordinate }

  def destinationCoordinate(routeRequestData: RouteRequestData): Option[Coordinate] =
    routeRequestData.remainingRoute.lastOption.map { _.linkSourceCoordinate }

  def currentLocationCoordinate(routeRequestData: RouteRequestData): Option[Coordinate] =
    routeRequestData.remainingRoute.headOption.map { _.linkSourceCoordinate }
}

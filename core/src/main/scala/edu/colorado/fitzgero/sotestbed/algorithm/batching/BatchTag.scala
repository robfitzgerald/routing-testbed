package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

sealed trait BatchTag

object BatchTag {

  val ValidBatchTags: Set[String] = Set("origin", "destination", "origin+destination", "current", "current+destination")

  def makeBatchTag(tagType: String, routeRequestData: RouteRequestData): Option[BatchTag] = {
    tagType.trim.toLowerCase match {
      case "origin"              => Some { OTag(routeRequestData) }
      case "destination"         => Some { DTag(routeRequestData) }
      case "origin+destination"  => Some { ODTag(routeRequestData) }
      case "current"             => Some { CTag(routeRequestData) }
      case "current+destination" => Some { CDTag(routeRequestData) }
      case _                     => None
    }
  }

  /**
    * uses the trip's origin (static) for grouping
    * @param routeRequestData data to create tags from
    */
  final case class OTag(routeRequestData: RouteRequestData) extends BatchTag {

    def tag(grid: CoordinateGrid, currentTime: SimTime, batchPathTimeDelay: SimTime): Option[String] =
      for {
        src <- originCoordinate(routeRequestData)
      } yield {
        val originTag: String = grid.getGridId(src.x, src.y)
        originTag
      }
  }

  /**
    * uses the trip's destination (static) for grouping
    * @param routeRequestData data to create tags from
    */
  final case class DTag(routeRequestData: RouteRequestData) extends BatchTag {

    def tag(grid: CoordinateGrid): Option[String] =
      for {
        dst <- destinationCoordinate(routeRequestData)
      } yield {
        val destinationTag: String = grid.getGridId(dst.x, dst.y)
        destinationTag
      }
  }

  /**
    * uses the trip's origin and destination (both static) for grouping
    * @param routeRequestData data to create tags from
    */
  final case class ODTag(routeRequestData: RouteRequestData) extends BatchTag {

    def tag(grid: CoordinateGrid, currentTime: SimTime, batchPathTimeDelay: SimTime): Option[String] =
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
    * @param routeRequestData data to create tags from
    */
  final case class CTag(routeRequestData: RouteRequestData) extends BatchTag {

    def tag(grid: CoordinateGrid, currentTime: SimTime, batchPathTimeDelay: SimTime): Option[String] =
      for {
        src <- currentLocationCoordinate(routeRequestData)
      } yield {
        val currentTag: String = grid.getGridId(src.x, src.y)
        currentTag
      }
  }

  /**
    * uses the trip's current location (dynamic) and destination (static) for grouping
    * @param routeRequestData data to create tags from
    */
  final case class CDTag(routeRequestData: RouteRequestData) extends BatchTag {

    def tag(grid: CoordinateGrid, currentTime: SimTime, batchPathTimeDelay: SimTime): Option[String] =
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

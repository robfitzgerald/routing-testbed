package edu.colorado.fitzgero.sotestbed.matsim.simulator

import scala.util.Try

import org.matsim.api.core.v01.{Coord, Id}
import org.matsim.api.core.v01.network.Link
import org.matsim.core.mobsim.qsim.QSim

import org.locationtech.proj4j._

object MATSimRouteToLineString {

  /**
    * print a path as a WKT LINESTRING
    * @param path the path
    * @param qSim the network
    * @return either a LINESTRING, or, None if there was an error
    */
  def apply(path: List[Id[Link]], qSim: QSim, destinationCRS: String = "EPSG:4326"): Option[String] = {

    val coords: List[ProjCoordinate] = path match {
      case Nil =>
        List.empty
      case linkId :: Nil =>
        // no special handling of "inner" coordinates when only one link id
        linkIdToCoords(linkId, qSim).getOrElse(List.empty)
      case _ =>
        // construct in such a way that we don't duplicate nodes
        unpackPathLongerThan2(path, qSim).getOrElse(List.empty)
    }

    if (coords.isEmpty) None
    else {
      val result: String = coords.map { asLineStringCoordinate }.mkString("\"LINESTRING (", ", ", ")\"")
      Some(result)
    }
  }

  /**
    * unpacks a longer list into coordinates without any repetition due to incident edges
    * @param path a path longer than 2
    * @param qSim the network
    * @return unique set of [[Coord]]s spanning the path
    */
  def unpackPathLongerThan2(path: List[Id[Link]], qSim: QSim): Option[List[ProjCoordinate]] = {

    def _unpack(remaining: List[Id[Link]] = path, resultOpt: Option[List[ProjCoordinate]] = Some(List.empty)): Option[List[ProjCoordinate]] = {
      resultOpt.flatMap { result =>
        remaining match {
          case last :: Nil =>
            linkIdToCoords(last, qSim).map { theseCoords =>
              theseCoords ::: result
            }
          case next :: tail =>
            srcCoordOfLinkId(next, qSim).flatMap { thisCoord =>
              val updatedResultOpt: Option[List[ProjCoordinate]] = Some(thisCoord +: result)
              _unpack(tail, updatedResultOpt)
            }
        }
      }
    }
    _unpack().map { _.reverse }
  }

  def matsimCoordToProjCoordinate(coord: Coord): ProjCoordinate = new ProjCoordinate(coord.getX, coord.getY)

  def linkIdToCoords(linkId: Id[Link], qSim: QSim): Option[List[ProjCoordinate]] = {
    val crs       = new CRSFactory()
    val transform = new BasicCoordinateTransform(crs.createFromName("EPSG:3857"), crs.createFromName("EPSG:4326"))

    val result: Option[List[ProjCoordinate]] = for {
      link    <- Try { qSim.getNetsimNetwork.getNetsimLink(linkId) }.toOption
      srcNode <- Try { matsimCoordToProjCoordinate(link.getLink.getFromNode.getCoord) }.toOption
      dstNode <- Try { matsimCoordToProjCoordinate(link.getLink.getToNode.getCoord) }.toOption
    } yield {
      var (srcLatLon, dstLatLon) = (new ProjCoordinate(), new ProjCoordinate())
      srcLatLon = transform.transform(srcNode, srcLatLon)
      dstLatLon = transform.transform(dstNode, dstLatLon)
      List(srcLatLon, dstLatLon)
    }
    result
  }

  def srcCoordOfLinkId(linkId: Id[Link], qSim: QSim): Option[ProjCoordinate] = {
    val crs       = new CRSFactory()
    val transform = new BasicCoordinateTransform(crs.createFromName("EPSG:3857"), crs.createFromName("EPSG:4326"))

    val result: Option[ProjCoordinate] = for {
      link    <- Try { qSim.getNetsimNetwork.getNetsimLink(linkId) }.toOption
      srcNode <- Try { matsimCoordToProjCoordinate(link.getLink.getFromNode.getCoord) }.toOption
    } yield {
      var srcLatLon = new ProjCoordinate()
      srcLatLon = transform.transform(srcNode, srcLatLon)
      srcLatLon
    }
    result
  }

  def asLineStringCoordinate(coord: ProjCoordinate): String = {
    f"${coord.x} ${coord.y}"
  }
}

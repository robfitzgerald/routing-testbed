package edu.colorado.fitzgero.sotestbed.matsim.simulator

import scala.util.Try

import org.matsim.api.core.v01.{Coord, Id}
import org.matsim.api.core.v01.network.Link
import org.matsim.core.mobsim.qsim.QSim

import org.locationtech.proj4j._
import scala.annotation.tailrec

object MATSimRouteToLineString {

  /**
    * print a path as a WKT LINESTRING
    * @param path the path
    * @param qSim the network
    * @return either a LINESTRING, or, None if there was an error
    */
  def apply(path: List[Id[Link]], qSim: QSim, destinationCRS: String = "EPSG:4326"): Option[String] = {

    // grab the source and destination nodes for each link and de-duplicate the result
    val allNodes = for {
      linkId <- path
      link = qSim.getNetsimNetwork.getNetsimLink(linkId).getLink
    } yield List(link.getFromNode, link.getToNode)
    val uniqueNodes = allNodes.flatten.distinctBy(_.getId)

    // map the coordinates into Lat/Lon (WGS84)
    val crs       = new CRSFactory()
    val transform = new BasicCoordinateTransform(crs.createFromName("EPSG:3857"), crs.createFromName("EPSG:4326"))
    val uniqueCoordsLatLon = uniqueNodes.map { n =>
      val mercator = new ProjCoordinate(n.getCoord.getX, n.getCoord.getY)
      transform.transform(mercator, new ProjCoordinate())
    }

    if (uniqueCoordsLatLon.isEmpty) None
    else {
      val result: String = uniqueCoordsLatLon
        .map { coord => f"${coord.x} ${coord.y}" }
        .mkString("\"LINESTRING (", ", ", ")\"")
      Some(result)
    }
  }
}

package edu.colorado.fitzgero.sotestbed.matsim.io.network.osm

sealed trait UnitedStatesUTMCoordinateSystem

object UnitedStatesUTMCoordinateSystem {
  // this was helpful: https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system#/media/File:Utm-zones-USA.svg
  // and so was this conversation: https://github.com/matsim-org/matsim-code-examples/wiki/faq-111778802

  case object UTM13N extends UnitedStatesUTMCoordinateSystem
  case object UTM18N extends UnitedStatesUTMCoordinateSystem

  def getCoordinateSystemWKT(utmCoordinateSystem: UnitedStatesUTMCoordinateSystem): String = {
    utmCoordinateSystem match {
      case UTM13N => "EPSG:32613" // Colorado east of Grand Junction ...
      case UTM18N => "EPSG:32618" // NYC, NY State, eastern seaboard ...
    }
  }
}

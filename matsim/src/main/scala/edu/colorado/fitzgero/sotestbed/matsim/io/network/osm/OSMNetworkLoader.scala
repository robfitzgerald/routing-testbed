package edu.colorado.fitzgero.sotestbed.matsim.io.network.osm

import java.nio.file.Path

import scala.collection.JavaConverters._
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import org.matsim.api.core.v01.network.Network
import org.matsim.core.network.NetworkUtils
import org.matsim.core.network.algorithms.NetworkCleaner
import org.matsim.core.network.io.NetworkWriter
import org.matsim.core.utils.geometry.CoordinateTransformation
import org.matsim.core.utils.geometry.transformations.TransformationFactory
import org.matsim.core.utils.io.OsmNetworkReader

object OSMNetworkLoader extends LazyLogging {

  /**
    * MATSim link lengths should be in meters. when using the OSM reader, the source
    * @see [[https://github.com/matsim-org/matsim-code-examples/blob/11.x/src/main/java/org/matsim/codeexamples/network/RunCreateNetworkFromOSM.java MATSim OSM example]]
    *
    * api documentation for MATSim 12 states that OsmNetworkReader, which we are using, is deprecated, and we should instead migrate to this contrib module:
    * @see [[https://github.com/Janekdererste/super-sonic-osm-network-reader]]
    *
    * @param sourceOSMNetworkFile the file we are loading
    * @param destinationCRS the crs we are converting to
    * @param destinationFileLocation if included, write the transformed file to disk
    * @param sourceCRS
    * @param useHighwayDefaults "Highway defaults are set to standard values, if true."
    * @param useVspAdjustments "Highway defaults are set to standard VSP values, if true."
    * @return
    */
  def createOSMNetworkWithUTMTransformation(sourceOSMNetworkFile: Path,
                                            destinationCRS: String,
                                            destinationFileLocation: Option[Path] = None,
                                            sourceCRS: String = TransformationFactory.WGS84,
                                            useHighwayDefaults: Boolean = true,
                                            useVspAdjustments: Boolean = false,
                                            removeResidentialLinks: Boolean = false,
                                            verbose: Boolean = true): Either[Throwable, Network] = {

    Try {
      val network                      = NetworkUtils.createNetwork
      val ct: CoordinateTransformation = TransformationFactory.getCoordinateTransformation(TransformationFactory.WGS84, destinationCRS)
      val onr                          = new OsmNetworkReader(network, ct, useHighwayDefaults, useVspAdjustments)

      // set residential the same as tertiary links
      onr.setHighwayDefaults(6, "unclassified", 1.0D, 6.944444444444445D, 1.0D, 600.0D)
      onr.setHighwayDefaults(7, "residential", 1.0D, 6.944444444444445D, 1.0D, 600.0D)

      // parse the provided file
      onr.parse(sourceOSMNetworkFile.toString)

      if (removeResidentialLinks) {

        val removedLinks: Int =
          network.getLinks.asScala.toMap.foldLeft(0) {
            case (count, (linkId, link)) =>
              link.getAttributes.getAsMap.asScala.get("type") match {
                case None => count
                case Some(linkType) =>
                  if (linkType.asInstanceOf[String] != "residential") count
                  else {
                    network.removeLink(linkId)
                    count + 1
                  }
              }
          }

        if (removedLinks > 0) logger.info(f"removed $removedLinks links with 'residential' classification")
      }

      // clean links
      new NetworkCleaner().run(network)

      // write to output file, if provided
      destinationFileLocation match {
        case None =>
          if (verbose) logger.info("not written to file as no destination file was provided")
        case Some(destinationFile) =>
          new NetworkWriter(network).write(destinationFile.toString)
      }

      if (verbose) logNetworkStats(network)

      network
    }
  }.toEither

  private[this] def logNetworkStats(network: Network): Unit = {
    network.getLinks.asScala
      .map { case (_, link) => link.getFreespeed }
      .groupBy { identity }
      .map {
        case (speed, instances) =>
          f"m/s=$speed mph=${speed * (3600.0 / 1609.0)}%.2f count ${instances.size}"
      }
      .foreach { row =>
        logger.info(row)
      }

    val totalLengthsInMeters: Double = network.getLinks.asScala.map {
      case (_, link) =>
        link.getLength // assuming we loaded a UTM map, it should be roughly in meters
    }.sum

    logger.info(f"total length in meters: $totalLengthsInMeters%.2f")
  }
}

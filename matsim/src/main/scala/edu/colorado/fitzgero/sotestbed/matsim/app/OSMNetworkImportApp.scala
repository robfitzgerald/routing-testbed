package edu.colorado.fitzgero.sotestbed.matsim.app

import java.nio.file.Path

import com.monovore.decline._
import cats.data.Writer
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.matsim.io.network.osm.OSMNetworkLoader

object OSMNetworkImportApp
    extends CommandApp(
      name = "so-testbed-osm-network-import",
      header = "imports an OpenStreetMap network XML file as a MATSim network",
      main = {

        val srcFileOpt: Opts[Path] =
          Opts.option[Path](long = "srcFile", short = "s", help = "URI of the OSM network file")
        val dstFileOpt: Opts[Path] =
          Opts.option[Path](long = "dstFile", short = "d", help = "URI to place the result file")
        val sourceCRSOpt: Opts[String] =
          Opts.option[String](long = "sourceCRS", help = "well-known CRS authority for the source file").withDefault("EPSG:4326")
        val destinationCRSOpt: Opts[String] =
          Opts
            .option[String](
              long = "destinationCRS",
              short = "c",
              help = "well-known CRS authority to apply as a transform to the input data, to a format that reflects meters in distance"
            )
            .withDefault("EPSG:3857") // see https://github.com/matsim-org/matsim-code-examples/wiki/faq-111614359

        (srcFileOpt, dstFileOpt, sourceCRSOpt, destinationCRSOpt)
          .mapN { (srcFile, dstFile, sourceCRS, destinationCRS) =>
            {
              println(srcFile.toString)
              println(dstFile.toString)
              OSMNetworkLoader.createOSMNetworkWithUTMTransformation(
                sourceOSMNetworkFile = srcFile,
                destinationCRS = destinationCRS,
                destinationFileLocation = Some { dstFile },
                sourceCRS = sourceCRS
              ) match {
                case Left(e) =>
                  throw e
                case Right(_) =>
                  println(s"finished importing, network can be found at $dstFile")
              }
            }
          }

      }
    )

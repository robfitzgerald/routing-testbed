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
        val monteCarloSamplesOpt: Opts[Int] =
          Opts
            .option[Int](long = "mcSamples",
                         short = "n",
                         help = "number of monte carlo samples of the free flow travel speed to get a lower-bound on travel time, default 50000")
            .withDefault(50000)

        val runMonteCarloSamplingOpt: Opts[Boolean] =
          Opts.flag(long = "runSampling", short = "m", help = "whether to run the monte carlo sampling of travel times, by default true").orFalse

        (srcFileOpt, dstFileOpt, sourceCRSOpt, destinationCRSOpt, monteCarloSamplesOpt, runMonteCarloSamplingOpt)
          .mapN {
            (srcFile, dstFile, sourceCRS, destinationCRS, n, runMCSampling) =>
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

                    if (runMCSampling) {
                      MonteCarloTravelTimeLowerBoundsOps.monteCarloTravelTimeLowerBoundSample(srcFile, 0, n) match {
                        case Left(e) =>
                          println(f"error while running monte carlo sampling")
                          throw new RuntimeException(e)
                        case Right(lowerBound) =>
                          println(f"monte carlo travel time estimate after $n samples is $lowerBound%.2f")
                      }
                    } else {
                      println(f"no monte carlo sampling value provided, skipping")
                    }
                }
              }
          }

      }
    )

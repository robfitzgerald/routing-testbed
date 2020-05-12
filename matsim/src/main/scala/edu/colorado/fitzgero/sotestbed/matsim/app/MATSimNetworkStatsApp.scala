package edu.colorado.fitzgero.sotestbed.matsim.app

import java.nio.file.Path

import scala.util.Try

import cats.implicits._

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.matsim.io.network.MATSimNetworkOps
import org.matsim.core.network.NetworkUtils

object MATSimNetworkStatsApp
    extends CommandApp(
      name = "so-testbed-matsim-network-stats",
      header = "gives data about networks",
      main = {

        val networkAOpt: Opts[Path] =
          Opts.option[Path](long = "networkA", short = "a", help = "a MATSim network file")
        val networkBOpt: Opts[Option[Path]] =
          Opts.option[Path](long = "networkB", short = "b", help = "a MATSim network file").orNone
        val graphOutDirOpt: Opts[Option[Path]] =
          Opts.option[Path](long = "outFile", short = "o", help = "out path for a WKT diff graph file").orNone

        (networkAOpt, networkBOpt, graphOutDirOpt)
          .mapN {
            (networkA, networkBOption, graphOutDirOption) =>
              {
                networkBOption match {
                  case None =>
                    Try {
                      val network = NetworkUtils.readNetwork(networkA.toString)
                      val stats   = MATSimNetworkOps.networkStats(network)
                      println(networkA.toString)
                      println(stats)
                      graphOutDirOption match {
                        case None =>
                          ()
                        case Some(graphOutDir) =>
                          MATSimNetworkOps.writeWKTNetwork(network, graphOutDir.resolve("wkt_graph.csv"))
                      }
                    } match {
                      case util.Failure(e) =>
                        println(s"failed to load network $networkA")
                        println(f"${e.getCause} ${e.getMessage}")
                        System.exit(1)
                      case util.Success(_) =>
                        ()
                    }
                  case Some(networkB) =>
                    Try {
                      val matsimNetworkA = NetworkUtils.readNetwork(networkA.toString)
                      val matsimNetworkB = NetworkUtils.readNetwork(networkB.toString)
                      val statsA         = MATSimNetworkOps.networkStats(matsimNetworkA)
                      val statsB         = MATSimNetworkOps.networkStats(matsimNetworkB)
                      println(networkA.toString)
                      println(statsA)
                      println(networkB.toString)
                      println(statsB)

                      val linkData = MATSimNetworkOps.countCommonEdgesByCommonCoordinates(matsimNetworkA, matsimNetworkB)
                      println(linkData)

                      graphOutDirOption match {
                        case None =>
                          ()
                        case Some(graphOutDir) =>
                          MATSimNetworkOps.writeWKTNetwork(matsimNetworkA, graphOutDir.resolve("wkt_graph_a.csv"))
                          MATSimNetworkOps.writeWKTNetwork(matsimNetworkB, graphOutDir.resolve("wkt_graph_b.csv"))
                          MATSimNetworkOps.createDiffWKTNetwork(matsimNetworkA, matsimNetworkB, graphOutDir.resolve("diff_graph.csv"))
                      }
                    } match {
                      case util.Failure(e) =>
                        println(s"failed to load network $networkA")
                        println(f"${e.getCause} ${e.getMessage}")
                        System.exit(1)
                      case util.Success(_) =>
                        ()
                    }
                }
              }
          }

      }
    )

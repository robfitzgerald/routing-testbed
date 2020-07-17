package edu.colorado.fitzgero.sotestbed.matsim.app

import java.nio.file.{Path, Paths}
import cats.implicits._

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.matsim.analysis.{OverallMetrics, PerformanceMetrics}

/**
  * takes one or two agentExperience.csv files and produces top-level
  * statistics for travel time, distance, and speed of agents
  */
object AgentExperienceBatchApp
    extends CommandApp(
      name = "Agent Experience Batch App",
      header = "top-level stats for experimental results",
      main = {
        val dirOpt: Opts[Path] =
          Opts.option[Path](long = "directory", short = "d", help = "compute outcomes from data in this directory")
        val soOnlyOpt: Opts[Boolean]          = Opts.flag("so-only", help = "only observe SO agents").orFalse
        val disimprovedOnlyOpt: Opts[Boolean] = Opts.flag("disimproved-only", help = "only stats of agents with worse travel time").orFalse

        (dirOpt, soOnlyOpt, disimprovedOnlyOpt).mapN {
          case (dir, soOnly, disimprovedOnly) =>
            // load all files in the directory

            if (!dir.toFile.isDirectory) {
              throw new IllegalArgumentException(s"$dir is not a directory")
            }

            println(f"node,file,${OverallMetrics.Header},${PerformanceMetrics.Header}")

            val outerResult: List[Unit] = for {
              subDir <- dir.toFile.listFiles.toList.filter { _.isDirectory }.sorted
              nodeName = subDir.getName // cluster node name
            } yield {

              val selfishFile = Paths.get(subDir.toString).resolve("selfish.csv").toFile

              val innerResult: List[Either[Exception, String]] = for {
                optimalFile <- subDir.listFiles.toList.sorted
                fileName = optimalFile.getName.substring(0, optimalFile.getName.length - 4) // hack to remove ".csv"
              } yield
                for {
                  overallMetrics     <- OverallMetrics(optimalFile)
                  performanceMetrics <- PerformanceMetrics.fromFiles(selfishFile, optimalFile, soOnly, disimprovedOnly)
                } yield f"$nodeName,$fileName,$overallMetrics,$performanceMetrics"

              for {
                r <- innerResult
              } {
                r match {
                  case Left(e) =>
                    println(s"Agent Experience App failure: ${e.getClass} ${e.getMessage} ${e.getCause}")
                  case Right(row) =>
                    println(f"$row")
                }
              }
            }

        }
      }
    )

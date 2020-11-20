package edu.colorado.fitzgero.sotestbed.matsim.app

import java.nio.file.Path

import cats.implicits._

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.matsim.analysis.AgentExperienceOps

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

        val refFileNameOpt: Opts[String] =
          Opts.option[String](long = "reference", short = "r", help = "name of file to build comparative statistics against")

        (dirOpt, refFileNameOpt).mapN {
          case (dir, refFileName) =>
            // load all files in the directory

            AgentExperienceOps.collectMetrics(dir, refFileName) match {
              case Left(e) =>
                throw e
              case Right(completeMetrics) =>
                val rows         = AgentExperienceOps.createFromUngroupedMetricResults(completeMetrics)
                val csvRows      = rows.map { _.outputString }
                val allPlotData  = rows.map { _.allData }
                val winPct       = rows.map { _.winPercent }
                val losePct      = rows.map { _.losePercent }
                val losePlotData = rows.map { _.loserData }

                // print csv to stdout
                println("\nCSV REPORT\n")
                println(AgentExperienceOps.AggregateOutputHeader)
                for { row <- csvRows } { println(row) }

                // print coords to stdout
                val (ttCoordsAll, distCoordsAll, _) = AgentExperienceOps.toLaTeXPlots(allPlotData)
                val (ttCoordsL, distCoordsL, _)     = AgentExperienceOps.toLaTeXPlots(losePlotData)
                val (winCoords, loseCoords)         = AgentExperienceOps.toLaTeXWinLosePlot(allPlotData)
                println("\n LaTeX PLOTS\n")
                println("\ntt-all")
                println(ttCoordsAll)
                println("\ndist-all")
                println(distCoordsAll)
                println("\nwin-all")
                println(winCoords)
                println("\nlose-all")
                println(loseCoords)
                println("\ntt-lose")
                println(ttCoordsL)
                println("\ndist-lose")
                println(distCoordsL)
            }
        }
      }
    )

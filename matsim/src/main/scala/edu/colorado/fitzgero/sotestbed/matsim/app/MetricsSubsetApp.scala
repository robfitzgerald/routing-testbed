package edu.colorado.fitzgero.sotestbed.matsim.app

import java.nio.file.Path

import cats.implicits._

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.matsim.analysis.{AgentExperienceOps, AgentMetrics, BatchDataOps, BatchMetrics}

/**
  * takes one or two agentExperience.csv files and produces top-level
  * statistics for travel time, distance, and speed of agents
  */
object MetricsSubsetApp
    extends CommandApp(
      name = "Metrics Subset App",
      header = "a single row combining agent and batch-level stats",
      main = {

        val dirOpt: Opts[Path] =
          Opts.option[Path](long = "directory", short = "d", help = "compute outcomes from data in this directory")

        val trialsOpt: Opts[Int] =
          Opts.option[Int](long = "trials", short = "t", help = "name of file to build comparative statistics against")

        val popSizeOpt: Opts[Int] =
          Opts.option[Int](long = "popSize", short = "p", help = "the population size of the experiments")

        val experimentNamesOpt: Opts[String] =
          Opts.option[String](long = "experimentNames", short = "e", help = "comma-delimited experiment names to query")

        (dirOpt, trialsOpt, experimentNamesOpt, popSizeOpt).mapN {
          case (dir, trials, experimentNames, popSize) =>
            // load all files in the directory

            val result = for {
              experimentName <- experimentNames.split(",")
            } yield {
              val analysisResult = for {
                completeMetrics <- AgentExperienceOps.collectAgentMetrics(dir, experimentName, popSize, trials)
                batchMetrics    <- BatchDataOps.collectBatchMetrics(dir, experimentName, trials)
              } yield {
                val rows    = AgentExperienceOps.createFromUngroupedMetricResults(completeMetrics)
                val csvRows = rows.map { _.outputString }
                (rows, csvRows, batchMetrics)
              }
              analysisResult match {
                case Left(e) =>
                  throw e
                case Right(printableOutputs) =>
                  printableOutputs
              }
            }

            println("\nMETRICS SUBSET REPORT\n")
            val header =
              s"experimentName,ttAll,ttσAll,speedAll,speedσAll,distAll,distσAll,ttLose,ttσLose,speedLose,speedσLose,distLose,distσLose,${BatchMetrics.Header}"
            println(header)
            for {
              (rawRows, _, batchMetricsList) <- result
            } {
              val batchAgg = batchMetricsList.foldLeft(BatchMetrics()) { _.add(_) }
              val stats = for {
                popAggMetrics <- rawRows
              } yield {
                //
                val experimentName = popAggMetrics.allData.experimentName
                val ttAll          = popAggMetrics.allData.ttNorm
                val ttσAll         = popAggMetrics.allData.ttStdev
                val speedAll       = popAggMetrics.allData.speedNorm
                val speedσAll      = popAggMetrics.allData.speedStdev
                val distAll        = popAggMetrics.allData.distNorm
                val distσAll       = popAggMetrics.allData.distStdev
                val ttLoser        = popAggMetrics.loserData.ttNorm
                val ttσLoser       = popAggMetrics.loserData.ttStdev
                val speedLoser     = popAggMetrics.loserData.speedNorm
                val speedσLoser    = popAggMetrics.loserData.speedStdev
                val distLoser      = popAggMetrics.loserData.distNorm
                val distσLoser     = popAggMetrics.loserData.distStdev
                val row =
                  f"$experimentName,$ttAll%.2f%%,$ttσAll%.2f%%,$speedAll%.2f%%,$speedσAll%.2f%%,$distAll%.2f%%,$distσAll%.2f%%,$ttLoser%.2f%%,$ttσLoser%.2f%%,$speedLoser%.2f%%,$speedσLoser%.2f%%,$distLoser%.2f%%,$distσLoser%.2f%%,${batchAgg.toString}"
                println(row)

              }
            }
//            val rawRows = result.flatMap { _._1 }.toList
//            AgentExperienceOps.toTikzPlot(rawRows.map { _.allData }, rawRows.map { _.loserData })
        }
      }
    )

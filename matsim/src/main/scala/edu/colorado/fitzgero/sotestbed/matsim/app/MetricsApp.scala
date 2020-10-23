package edu.colorado.fitzgero.sotestbed.matsim.app

import java.nio.file.Path

import cats.implicits._

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.matsim.analysis.{AgentExperienceOps, AgentMetrics, BatchDataOps, BatchMetrics}

/**
  * takes one or two agentExperience.csv files and produces top-level
  * statistics for travel time, distance, and speed of agents
  */
object MetricsApp
    extends CommandApp(
      name = "Agent Experience Batch App",
      header = "top-level stats for experimental results",
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
                completeMetrics <- AgentExperienceOps.collectMetrics2(dir, experimentName, popSize, trials)
                batchMetrics    <- BatchDataOps.collectBatchMetrics(dir, experimentName, trials)
              } yield {
                val rows    = AgentExperienceOps.createFromUngroupedMetricResults(completeMetrics)
                val csvRows = rows.map { _._1 }
                (rows, csvRows, batchMetrics)
              }
              analysisResult match {
                case Left(e) =>
                  throw e
                case Right(printableOutputs) =>
                  printableOutputs
              }
            }

            println("\nCSV TOP-LEVEL AGENT EXPERIENCE REPORT\n")
            println(AgentExperienceOps.AggregateOutputHeader)
            for {
              (_, csvRows, _) <- result
            } {
              for { row <- csvRows } { println(row) }
            }

            println("\nCSV DIS-AGGREGATE AGENT EXPERIENCE REPORT\n")
            println(AgentMetrics.Header)
            for {
              (completeMetrics, _, _) <- result
            } {
              for { row <- completeMetrics } { println(row) }
            }

            println("\nCSV BATCH REPORT\n")
            println(BatchMetrics.Header)
            for {
              (_, _, batchMetrics) <- result
            } {
              val aggregate = batchMetrics.foldLeft(BatchMetrics()) { _.add(_) }
              println(aggregate.toString)
            }

            println("\nCSV DIS-AGGREGATE BATCH REPORT\n")
            println(BatchMetrics.Header)
            for {
              (_, _, batchMetrics) <- result
            } {
              for { row <- batchMetrics } { println(row) }
            }
        }
      }
    )

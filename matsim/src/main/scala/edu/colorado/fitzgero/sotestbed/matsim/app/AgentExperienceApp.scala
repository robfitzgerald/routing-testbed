package edu.colorado.fitzgero.sotestbed.matsim.app

import java.nio.file.Path

import cats.implicits._

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.matsim.analysis.{OverallMetrics, PerformanceMetrics}

/**
  * takes one or two agentExperience.csv files and produces top-level
  * statistics for travel time, distance, and speed of agents
  */
object AgentExperienceApp
    extends CommandApp(
      name = "Agent Experience App",
      header = "top-level stats for experimental results",
      main = {
        val agentExperienceFilePathOpt: Opts[Path] =
          Opts.option[Path](long = "this", short = "t", help = "pull stats on this outcome")
        val refAgentExeperienceFilePathOpt: Opts[Option[Path]] =
          Opts
            .option[Path](long = "reference", short = "r", help = "optionally comparative stats against this ref outcome with same population")
            .orNone

        (agentExperienceFilePathOpt, refAgentExeperienceFilePathOpt).mapN {
          (agentExpFilePath, refFilePathOption) =>
            {
              val result = refFilePathOption match {
                case None =>
                  for {
                    overallMetrics <- OverallMetrics(agentExpFilePath.toFile)
                  } yield {
                    println(overallMetrics)
                  }
                case Some(refFilePath) =>
                  for {
                    overallMetrics     <- OverallMetrics(agentExpFilePath.toFile)
                    performanceMetrics <- PerformanceMetrics.fromFiles(refFilePath.toFile, agentExpFilePath.toFile)
                  } yield {
                    println(OverallMetrics.Header)
                    println(overallMetrics)
                    println(PerformanceMetrics.Header)
                    println(performanceMetrics)
                  }
              }
              result match {
                case Left(e) =>
                  println("Agent Experience App failure")
                  throw e
                case Right(_) =>
              }
            }
        }
      }
    )

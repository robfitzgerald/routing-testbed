package edu.colorado.fitzgero.sotestbed.matsim.app

import java.nio.file.{Files, Path}

import scala.util.Random

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.matsim.config.generator.GeneratorAppOps
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig._
import edu.colorado.fitzgero.sotestbed.matsim.runner.MATSimExperimentRunner3
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population.PopSamplingAlgorithm
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import cats.effect.unsafe.implicits.global
import java.nio.file.Paths

object MATSimExperiment2021App
    extends CommandApp(
      name = "so-testbed-experiment-app",
      header = "run a trial from a HOCON description",
      main = {
        val configFileOpt: Opts[Path] =
          Opts.option[Path](
            long = "config",
            short = "c",
            help = "hocon configuration for an experiment; should begin with a number"
          )
        val outerResult = for {
          configFile <- configFileOpt
        } yield {

          // parse the HOCON configuration file, set up for this run
          val matsimRunConfigOrError = ConfigSource
            .file(configFile.toFile)
            .load[MATSimConfig]
            .left
            .map { error => new Error(s"failed to parse configuration\n${error.prettyPrint(2)}") }
            .map {
              config =>
                val popFileName: String = config.population.filename()

                // ack, we need to set the batchName first before using the updated IO object to set
                // the population file path.. who made this crap? :-D
                val ioWithBatchName = config.io.copy(
                  batchName = GeneratorAppOps.leadingEnumerationFrom(configFile.toFile)
                )
                val cwdPath     = Paths.get("").toAbsolutePath
                val popFilePath = cwdPath.resolve(ioWithBatchName.batchLoggingDirectory).resolve(popFileName).toFile

                println(f"setting population filepath: $popFilePath")
                val updatedMATSimConfig = config.copy(io = ioWithBatchName.copy(populationFile = popFilePath))
                MATSimRunConfig(updatedMATSimConfig)
            }

          val directoryUnusedOrError = for {
            matsimRunConfig <- matsimRunConfigOrError
            _ <- if (Files.isDirectory(matsimRunConfig.experimentLoggingDirectory)) {
              Left(new Error(s"output directory already exists: ${matsimRunConfig.experimentLoggingDirectory}"))
            } else Right(())
          } yield ()

          // write the OS output directory and population file if missing
          val populationCreatedOrError = for {
            matsimRunConfig <- matsimRunConfigOrError
            _               <- directoryUnusedOrError
            popFilePath     <- PopulationSamplingOps.buildPopulationIfMissing(matsimRunConfig)
            _ = println(f"using population file at $popFilePath")
          } yield ()

          // run our experiment
          val innerResult = for {
            _               <- populationCreatedOrError
            matsimRunConfig <- matsimRunConfigOrError
            runner = MATSimExperimentRunner3(matsimRunConfig)
            _      = runner.run().unsafeRunSync
          } yield runner

          innerResult match {
            case Left(error) =>
              throw error
            case Right(_) =>
          }
        }
        outerResult
      }
    )

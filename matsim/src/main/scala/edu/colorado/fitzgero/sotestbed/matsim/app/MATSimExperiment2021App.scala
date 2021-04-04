package edu.colorado.fitzgero.sotestbed.matsim.app

import java.nio.file.{Files, Path}

import scala.util.Random

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.matsim.config.generator.GeneratorAppOps
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig._
import edu.colorado.fitzgero.sotestbed.matsim.runner.MATSimExperimentRunner3
import pureconfig.ConfigSource
import pureconfig.generic.auto._

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

          // parse the HOCON configuration file
          val matsimConfigOrError = ConfigSource
            .file(configFile.toFile)
            .load[MATSimConfig]
            .left
            .map { error => new Error(s"failed to parse configuration\n${error.prettyPrint(2)}") }
            .map {
              config =>
                val popFileName: String = s"population-${config.population.size}.xml"

                // ack, we need to set the batchName first before using the updated IO object to set
                // the population file path.. who made this crap? :-D
                val ioWithBatchName = config.io.copy(
                  batchName = GeneratorAppOps.leadingEnumerationFrom(configFile.toFile)
                )
                val popFilePath = ioWithBatchName.batchLoggingDirectory.resolve(popFileName).toFile
                config.copy(io = ioWithBatchName.copy(populationFile = popFilePath))
            }

          // make some transformations to fit our config into the legacy expectations
          val matsimRunConfigOrError = for {
            config <- matsimConfigOrError
          } yield {
            val popSize = config.population.size
            val scenarioData = MATSimRunConfig.ScenarioData(
              algorithm = config.algorithm.name,
              variationName = popSize.toString,
              popSize = popSize,
              trialNumber = 0,
              headerColumnOrder = List.empty,
              scenarioParameters = Map.empty
            )
            MATSimRunConfig(config, scenarioData)
          }

          // set up our random generator, a function of the current time and 3 algorithm parameters
          val randomOrError = for {
            matsimRunConfig <- matsimRunConfigOrError
          } yield {
            val seed: Long = matsimRunConfig.population.size +
              matsimRunConfig.routing.batchWindow.value +
              (matsimRunConfig.routing.adoptionRate * 1000).toInt +
              System.currentTimeMillis
            new Random(seed)
          }

          // write the OS output directory and population file if missing
          val populationCreatedOrError = for {
            matsimRunConfig <- matsimRunConfigOrError
            random          <- randomOrError
          } yield {
            // handle OS interaction
            Files.createDirectories(matsimRunConfig.experimentLoggingDirectory)
            Files.createDirectories(matsimRunConfig.io.batchLoggingDirectory)

            // create population file if missing
            if (!matsimRunConfig.io.populationFile.isFile) {
              MATSimPopulationRunner.generateUniformPopulation(
                networkFile = matsimRunConfig.io.matsimNetworkFile,
                polygonFileOption = matsimRunConfig.io.populationPolygonFile,
                popFileDestination = matsimRunConfig.io.populationFile,
                popSize = matsimRunConfig.population.size,
                adoptionRate = matsimRunConfig.routing.adoptionRate,
                workActivityMinTime = matsimRunConfig.population.workActivityMinTime,
                workActivityMaxTime = matsimRunConfig.population.workActivityMaxTime,
                workDurationHours = matsimRunConfig.population.workDurationHours,
                seed = Some { random.nextInt }
              )
            }
            ()
          }

          // run our experiment
          val innerResult = for {
            _               <- populationCreatedOrError
            matsimRunConfig <- matsimRunConfigOrError
            random          <- randomOrError
            runner = MATSimExperimentRunner3(matsimRunConfig, random.nextLong)
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

package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.File
import java.nio.file.{Files, Path}

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.runner.MATSimExperimentRunner3
import pureconfig.ConfigSource
import cats.effect._
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig._
import pureconfig.generic.auto._

import com.monovore.decline._
import com.monovore.decline.effect._

object RepeaterApp
    extends CommandIOApp(
      name = "so-testbed-experiment-repeater-app",
      header = "run a trial repeatedly from a HOCON description"
    ) {

  val configFileOpt: Opts[Path] =
    Opts.option[Path](
      long = "config",
      short = "c",
      help = "hocon configuration for an experiment; should begin with a number"
    )

  val startingNumberOpt: Opts[Int] =
    Opts.option[Int](
      long = "start",
      short = "s",
      help = "start output directory name with counter from this value"
    )

  val seedOpt: Opts[Long] = Opts.option[Long](long = "seed", help = "random seed").withDefault(System.currentTimeMillis)

  def main: Opts[IO[ExitCode]] = {

    (configFileOpt, startingNumberOpt, seedOpt).mapN {
      case (configFile, startingNumber, seed) =>
        val random = new Random(seed)

        println(s"if this dies, make sure you're not writing to an existing directory!!! :-O")

        createTemplateConfig(configFile.toFile).flatMap { config =>
          startingNumber.iterateForeverM { iteration =>
            val itConf = prepareRunConfig(config, iteration)
            val runner = MATSimExperimentRunner3(itConf, random.nextLong)
            println(s"--- running training repeater iteration $iteration")
            for {
              _ <- buildPopulationIfMissing(itConf, random)
              _ <- runner.run()
            } yield iteration + 1
          }
        }
    }
  }

  def createTemplateConfig(configFile: File): IO[MATSimConfig] = {
    val confOrError =
      ConfigSource
        .file(configFile)
        .load[MATSimConfig]
        .left
        .map { error => new Error(s"failed to parse configuration\n${error.prettyPrint(2)}") }

    IO.fromEither(confOrError)
  }

  def prepareRunConfig(config: MATSimConfig, iteration: Int): MATSimRunConfig = {
    val popFileName: String = s"population-${config.population.size}.xml"

    // ack, we need to set the batchName first before using the updated IO object to set
    // the population file path.. who made this crap? :-D
    val ioWithBatchName = config.io.copy(
//      batchName = f"qmix$iteration"
      batchName = 0.toString
    )
    val popFilePath = ioWithBatchName.batchLoggingDirectory.resolve(popFileName).toFile
    val updatedConf = config.copy(io = ioWithBatchName.copy(populationFile = popFilePath))

    val popSize = updatedConf.population.size
    val scenarioData = MATSimRunConfig.ScenarioData(
//      algorithm = updatedConf.algorithm.name,
      algorithm = s"repeated-$iteration",
      variationName = popSize.toString,
      popSize = popSize,
      trialNumber = 0,
      headerColumnOrder = List.empty,
      scenarioParameters = Map.empty
    )
    MATSimRunConfig(updatedConf, scenarioData)
  }

  def buildPopulationIfMissing(matsimRunConfig: MATSimRunConfig, rng: Random): IO[Unit] = {
    val exists = matsimRunConfig.io.populationFile.isFile
    if (exists) IO.pure(())
    else {
      val result = MATSimPopulationRunner
        .generateUniformPopulation(
          networkFile = matsimRunConfig.io.matsimNetworkFile,
          polygonFileOption = matsimRunConfig.io.populationPolygonFile,
          popFileDestination = matsimRunConfig.io.populationFile,
          popSize = matsimRunConfig.population.size,
          adoptionRate = matsimRunConfig.routing.adoptionRate,
          workActivityMinTime = matsimRunConfig.population.workActivityMinTime,
          workActivityMaxTime = matsimRunConfig.population.workActivityMaxTime,
          workDurationHours = matsimRunConfig.population.workDurationHours,
          seed = Some { rng.nextInt }
        )
        .left
        .map { msg => new Throwable(msg.toString) }
      IO.fromEither(result)
    }
  }
}

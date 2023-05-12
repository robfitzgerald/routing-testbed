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
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population.PopSamplingAlgorithm

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

  val nameOpt: Opts[String] =
    Opts
      .option[String](
        long = "name",
        short = "n",
        help = "name, used when creating output directories for MATSim runs"
      )
      .withDefault("repeated")

  val seedOpt: Opts[Option[Int]] =
    Opts.option[Int](long = "seed", help = "random seed value (multiplied against iteration #)").orNone

  def main: Opts[IO[ExitCode]] = {

    (configFileOpt, startingNumberOpt, nameOpt, seedOpt).mapN {
      case (configFile, startingNumber, name, seed) =>
        createTemplateConfig(configFile.toFile).flatMap { config =>
          startingNumber.iterateForeverM { iteration =>
            // val itConf = prepareRunConfig(config, iteration, name)
            val iterationSeed = seed.map(_ * iteration)
            val itConf        = MATSimRunConfig(config, trial = Some(iteration), matsimRunSeed = iterationSeed)
            val runner        = MATSimExperimentRunner3(itConf)
            println(s"--- running training repeater iteration $iteration")
            for {
              // _ <- mkDirsFromFile(itConf.io.populationFile)
              _ <- IO.fromEither(PopulationSamplingOps.buildPopulationIfMissing(itConf))
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

//   def prepareRunConfig(config: MATSimConfig, iteration: Int, name: String): MATSimRunConfig = {
//     val popFileName: String = config.population.filename(iteration)
//     val popFileName: String = s"population-$iteration-${config.population.size}.xml"

//     // ack, we need to set the batchName first before using the updated IO object to set
//     // the population file path.. who made this crap? :-D
//     val ioWithBatchName = config.io.copy(
// //      batchName = f"qmix$iteration"
//       batchName = 0.toString
//     )
//     val popFilePath = ioWithBatchName.batchLoggingDirectory.resolve(popFileName).toFile
//     val updatedConf = config.copy(io = ioWithBatchName.copy(populationFile = popFilePath))

//     val popSize = updatedConf.population.size
//     val scenarioData = MATSimRunConfig.ScenarioData(
// //      algorithm = updatedConf.algorithm.name,
//       algorithm = s"$name-$iteration",
//       variationName = popSize.toString,
//       popSize = popSize,
//       trialNumber = 0,
//       headerColumnOrder = List.empty,
//       scenarioParameters = Map.empty
//     )
//     MATSimRunConfig(updatedConf, scenarioData)
//   }

  /**
    * https://stackoverflow.com/a/4040667/4803266
    */
  // def mkDirsFromFile(file: File): IO[Unit] = {
  //   val parent = file.getParentFile
  //   if (parent != null && !parent.exists() && !parent.mkdirs()) {
  //     IO.raiseError(new IllegalStateException("Couldn't create dir: " + parent))
  //   } else IO.unit
  // }

  // val exists = matsimRunConfig.io.populationFile.isFile
  // println(f"required population file already exist? $exists")
  // if (exists) IO.pure(())
  // else {
  //   val result = MATSimPopulationRunner
  //     .generateUniformPopulation(
  //       networkFile = matsimRunConfig.io.matsimNetworkFile,
  //       polygonFileOption = matsimRunConfig.io.populationPolygonFile,
  //       popFileDestination = matsimRunConfig.io.populationFile,
  //       popSize = matsimRunConfig.population.size,
  //       adoptionRate = matsimRunConfig.routing.adoptionRate,
  //       workActivityMinTime = matsimRunConfig.population.workActivityMinTime,
  //       workActivityMaxTime = matsimRunConfig.population.workActivityMaxTime,
  //       workDurationHours = matsimRunConfig.population.workDurationHours,
  //       seed = Some { rng.nextInt }
  //     )
  //     .left
  //     .map { msg => new Throwable(msg.toString) }
  //   IO.fromEither(result)
  // }
  // }
}

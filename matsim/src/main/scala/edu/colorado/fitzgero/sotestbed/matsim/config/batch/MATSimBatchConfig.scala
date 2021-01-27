package edu.colorado.fitzgero.sotestbed.matsim.config.batch

import java.io.{File, IOError}
import java.nio.file.{Files, Path, Paths}

import scala.annotation.tailrec
import scala.util.Try

import com.typesafe.config.{Config, ConfigValueFactory}
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPAlgorithm
import edu.colorado.fitzgero.sotestbed.config.{BatchingFunctionConfig, KSPAlgorithmConfig}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig
import pureconfig._
import pureconfig.error.{ConfigReaderFailures, ThrowableFailure}
import pureconfig.generic.auto._
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig.localDateConvert

object MATSimBatchConfig {
  // provides the ability to generate MATSimConfigs from a range of parameter values

  // the config keys that are specific to the batch config and not related to the experiment parameters
  val ignoredBatchKeys: Set[String] = Set("scenarioBasePath", "output-base-directory", "type")

  /**
    * a variation hint is produced each time we set up a combination of parameter values from a
    * batch config. this name-ifies the hint, which is just a list of strings containing
    * key/value information. this is made into a string that is file-system friendly here.
    * @param variationHint the variation hint
    * @return a variation name which is OS-friendly
    */
  def createVariationName(variationHint: Map[String, String]): String = {
    variationHint
      .filterNot { case (_, v) => Files.exists(Paths.get(v)) }
      .map { case (k, v) => s"$k=${v.replaceAll("^[ .]+|\\.+$|\\.(?=[^.]*\\.[^.]*$)|[?\\\\/:;]", "_")}" } // v -> k.replaceAll("[^a-zA-Z0-9._]+", "_")
      .mkString("_")
      .trim
  }

  def createVariationNameWithFallback(matsimConfig: MATSimConfig, popSize: Int, config: Config): String = {
    if (config.hasPath("name")) {
      config.getString("name")
    } else {
      createVariationNameV2(matsimConfig, popSize)
    }
  }

  /**
    * just highlighting the most important parameters and returning their values in lexi order
    * @param config
    * @return
    */
  def createVariationNameV2(config: MATSimConfig, popSize: Int): String = {
    val populationSize: String = popSize.toString
    val adoptionRate: String   = config.routing.adoptionRate.toString
    val batchWindow: String    = config.routing.batchWindow.value.toString
    val k: String = config.algorithm match {
      case _: MATSimConfig.Algorithm.Selfish        => "0"
      case so: MATSimConfig.Algorithm.SystemOptimal => so.kspAlgorithm.k.toString
    }
    val theta: String = config.algorithm match {
      case _: MATSimConfig.Algorithm.Selfish => "0"
      case so: MATSimConfig.Algorithm.SystemOptimal =>
        so.kspAlgorithm match {
          case KSPAlgorithmConfig.SvpLoSync(_, theta, _, _) => theta.value.toString
        }
    }
    val batchingFunction: String = config.algorithm match {
      case _: MATSimConfig.Algorithm.Selfish => "0"
      case so: MATSimConfig.Algorithm.SystemOptimal =>
        so.batchingFunction match {
          case _: BatchingFunctionConfig.Random                  => "0"
          case bf: BatchingFunctionConfig.CoordinateGridGrouping => bf.batchType
        }
    }
    val gridCellSideLength: String = config.algorithm match {
      case _: MATSimConfig.Algorithm.Selfish => "0"
      case so: MATSimConfig.Algorithm.SystemOptimal =>
        so.batchingFunction match {
          case _: BatchingFunctionConfig.Random                  => "0"
          case bf: BatchingFunctionConfig.CoordinateGridGrouping => bf.gridCellSideLength.toString
        }
    }
    val maxPathAssignments: String    = config.routing.maxPathAssignments.toString
    val minReplanningWaitTime: String = config.routing.minimumReplanningWaitTime.value.toString
    s"p=$populationSize-a=$adoptionRate-b=$batchWindow-k=$k-t=$theta-bf=$batchingFunction-sqlen=$gridCellSideLength-mP=$maxPathAssignments-mRWT=$minReplanningWaitTime"
  }

  def createVariationNameV3(config: MATSimConfig, popSize: Int): String = {
    config.hashCode().toString
  }

  final case class Variation(
    config: Config,
    configReaderResult: ConfigReader.Result[MATSimConfig],
    variationHint: Map[String, String],
    populationSize: Int
  )

  /**
    * parses a batch config into usable experiment configurations
    *
    * @param batchConfigFile the batch config
    * @param scenarioConfigFilePath the directory related to a scenario containing configs for each algorithm
    * @return either errors, or all variations of MATSimConfig requested by
    *         the batch config along with the variation hints for those configs
    */
  def readBatchConfig(batchConfigFile: File, scenarioConfigFilePath: Path): Either[IOError, List[Variation]] = {

    @tailrec
    def appendMetaConfigEntry(config: Config, variation: List[(String, String)]): Config = {
      if (variation.isEmpty) {
        // base case - all variations have been applied
        config
      } else {
        // override the config at this variation's path with this variation's value
        val (thisPath, thisValue) = variation.head
        val nextConfig: Config    = config.withValue(thisPath.trim, ConfigValueFactory.fromAnyRef(thisValue.trim))
        appendMetaConfigEntry(nextConfig, variation.tail)
      }
    }

    // load batch config file
    ConfigSource.file(batchConfigFile).config() match {
      case Left(error) =>
        Left(new IOError(new Throwable(error.prettyPrint())))
      case Right(batchConfParsed) =>
        val configVariations: List[Variation] = for {
          variation <- MultiSetConfigIterator(batchConfParsed).allCombinations
        } yield {

          // the batch config should have an algorithm.name for each combination,
          // used to look up the default config for this algorithm
          variation.toMap.get("algorithm.name") match {
            case None =>
              val error: ConfigReaderFailures =
                ConfigReaderFailures(
                  ThrowableFailure(new IOError(new Throwable("cannot find algorithm.name in batch file")), None)
                )
              Variation(batchConfParsed, Left(error), Map.empty, 0)

            case Some(algorithmName) =>
              val defaultConfigFile: Path = scenarioConfigFilePath.resolve(s"$algorithmName.conf".trim)

              if (!Files.isRegularFile(defaultConfigFile)) {
                val error: ConfigReaderFailures = ConfigReaderFailures(
                  ThrowableFailure(
                    new IOError(
                      new Throwable(s"config file for algorithm variation does not exist: $defaultConfigFile")
                    ),
                    None
                  )
                )
                Variation(batchConfParsed, Left(error), Map.empty, 0)
              } else {

                // build the default config
                ConfigSource.file(defaultConfigFile).config() match {
                  case Left(error)                => Variation(batchConfParsed, Left(error), Map.empty, 0)
                  case Right(defaultConfigParsed) =>
                    // apply all variations for this combination of attributes for the batch config
                    val thisVariationConfig: Config = appendMetaConfigEntry(defaultConfigParsed, variation)

                    // find the pop size for this variation
                    Try {
                      thisVariationConfig.getValue("pop.size").unwrapped.toString.trim.toInt
                    } match {
                      case util.Failure(error) =>
                        val configReaderFailure: ConfigReaderFailures = ConfigReaderFailures(
                          ThrowableFailure(new IOError(error), None)
                        )
                        Variation(thisVariationConfig, Left(configReaderFailure), Map.empty, 0)
                      case util.Success(popSize) =>
                        val thisVariationMATSimConfig: ConfigReader.Result[MATSimConfig] =
                          ConfigSource.fromConfig(thisVariationConfig).load[MATSimConfig]

                        // produces a map of the parameter values we are choosing
                        val thisVariationHintMap: Map[String, String] =
                          variation.flatMap {
                            case (key, value) =>
                              key.split('.').lastOption match {
                                case None         => if (ignoredBatchKeys(key)) None else Some { key       -> value }
                                case Some(suffix) => if (ignoredBatchKeys(suffix)) None else Some { suffix -> value }
                              }
                          }.toMap

                        Variation(thisVariationConfig, thisVariationMATSimConfig, thisVariationHintMap, popSize)
                    }

                }
              }
          }
        }
        Right(configVariations)
    }
  }
}

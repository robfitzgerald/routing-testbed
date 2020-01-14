package edu.colorado.fitzgero.sotestbed.matsim.config.batch

import java.io.{File, IOError}
import java.nio.file.{Files, Path}

import scala.annotation.tailrec

import com.typesafe.config.{Config, ConfigValueFactory}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig
import pureconfig._
import pureconfig.error.{ConfigReaderFailures, ThrowableFailure}
import pureconfig.generic.auto._

object MATSimBatchConfig {
  // provides the ability to generate MATSimConfigs from a range of parameter values

  def createVariationName(variationHint: List[String]): String = {
    variationHint
      .map{ _.replaceAll("[^a-zA-Z0-9._]+", "_") }
      .mkString("_")
      .trim
  }

  def readBatchConfig(batchConfigFile: File,
                      scenarioConfigFilePath: Path): Either[IOError, List[(ConfigReader.Result[MATSimConfig], List[String])]] = {

    @tailrec
    def appendMetaConfigEntry(config: Config, variation: List[(String, String)]): Config = {
      if (variation.isEmpty) config
      else {
        val (thisPath, thisValue) = variation.head
        val nextConfig: Config    = config.withValue(thisPath, ConfigValueFactory.fromAnyRef(thisValue))
        appendMetaConfigEntry(nextConfig, variation.tail)
      }
    }

    ConfigSource.file(batchConfigFile).config() match {
      case Left(error) =>
        Left(new IOError(new Throwable(error.prettyPrint())))
      case Right(batchConfParsed) =>
        val configVariations: List[(ConfigReader.Result[MATSimConfig], List[String])] = for {
          variation <- MultiSetConfigIterator(batchConfParsed).allCombinations
        } yield {
          variation.toMap.get("algorithm.name") match {
            case None =>
              val error: ConfigReaderFailures =
                ConfigReaderFailures(ThrowableFailure(new IOError(new Throwable("cannot find algorithm.name in batch file")), None))
              (Left(error), List.empty)

            case Some(algorithmName) =>
              val defaultConfigFile: Path = scenarioConfigFilePath.resolve(s"$algorithmName.conf".trim)

              if (!Files.isRegularFile(defaultConfigFile)) {
                val error: ConfigReaderFailures = ConfigReaderFailures(
                  ThrowableFailure(new IOError(new Throwable(s"config file for algorithm variation does not exist: $defaultConfigFile")), None)
                )
                (Left(error), List.empty)
              } else {

                ConfigSource.file(defaultConfigFile).config() match {
                  case Left(error) => (Left(error), List.empty)
                  case Right(defaultConfigParsed) =>
                    val thisVariationConfig: Config = appendMetaConfigEntry(defaultConfigParsed, variation)
                    val thisVariationMATSimConfig: ConfigReader.Result[MATSimConfig] =
                      ConfigSource.fromConfig(thisVariationConfig).load[MATSimConfig]
                    val thisVariationHint: List[String] =
                      variation
                        .filter{ case(k, _) => !k.contains("algorithm.name") }
                        .map {
                          case (key, value) =>
                            key.split('.').headOption match {
                              case None         => s"${key.head}=$value"
                              case Some(suffix) => s"${suffix.head}=$value"
                            }
                        }
                    (thisVariationMATSimConfig, thisVariationHint)
                }
              }
          }
        }
        Right(configVariations)
    }
  }
}

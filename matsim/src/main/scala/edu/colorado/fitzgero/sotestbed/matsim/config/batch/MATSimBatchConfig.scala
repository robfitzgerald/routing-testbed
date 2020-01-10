package edu.colorado.fitzgero.sotestbed.matsim.config.batch

import java.io.{File, IOError}

import scala.annotation.tailrec

import com.typesafe.config.{Config, ConfigValueFactory}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig
import pureconfig._

object MATSimBatchConfig {
  // provides the ability to generate MATSimConfigs from a range of parameter values

  def readBatchConfig(batchConfigFile: File, defaultConfigFile: File): Either[IOError, List[ConfigReader.Result[MATSimConfig]]] = {

    @tailrec
    def appendMetaConfigEntry(config: Config, variation: List[(String, String)]): Config = {
      if (variation.isEmpty) config
      else {
        val (thisPath, thisValue) = variation.head
        val nextConfig: Config = config.withValue(thisPath, ConfigValueFactory.fromAnyRef(thisValue))
        appendMetaConfigEntry(nextConfig, variation.tail)
      }
    }

    val result: List[ConfigReader.Result[MATSimConfig]] = for {
      batchConfig <- ConfigSource.file(batchConfigFile).config().toOption.toList
      variation <- MultiSetConfigIterator(batchConfig).allCombinations
      defaultConfig <- ConfigSource.file(defaultConfigFile).config().toOption.toList
    } yield {
      val thisVariationConfig: Config = appendMetaConfigEntry(defaultConfig, variation)
      ConfigSource.fromConfig(thisVariationConfig).load[MATSimConfig]
    }

    Right(result)
  }
}

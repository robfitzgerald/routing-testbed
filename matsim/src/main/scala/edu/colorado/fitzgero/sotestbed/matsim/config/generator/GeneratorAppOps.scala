package edu.colorado.fitzgero.sotestbed.matsim.config.generator

import java.io.File
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import scala.util.matching.Regex

import cats.implicits._
import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging

object GeneratorAppOps extends LazyLogging {

  val matsimResourceDirectory: Path = Paths.get("matsim/src/main/resources/matsim-resources-2021")

  /**
    * copy all MATSim dependencies into the experiment configuration directory
    * @param outDir the directory to write to
    * @param scenarios each scenario we are copying
    * @return copy of the scenario network.xml files and one common default config.xml file for MATSim runs
    */
  def copyDependencies(outDir: Path, scenarios: List[Scenario]): IO[Unit] = {
    val networkCopyPrograms: List[IO[(Scenario, Path)]] = for {
      scenario <- scenarios
      networkFilePath = scenario.networkFilePath
    } yield {
      IO {
        val networkFileName = networkFilePath.getFileName
        val dst             = outDir.resolve(networkFileName)
        Files.copy(networkFilePath, dst, StandardCopyOption.REPLACE_EXISTING)
        (scenario, dst)
      }
    }

    val logNetworkLocations: IO[Unit] = for {
      outPaths <- networkCopyPrograms.sequence
    } yield {
      outPaths.foreach { case (s, p) => logger.info(f"copied network file for scenario $s to $p") }
    }

    val configCopyProgram: IO[Unit] = for {
      _ <- logNetworkLocations
    } yield {
      val configFilename = "matsim-config.xml"
      val src            = matsimResourceDirectory.resolve(configFilename)
      val dst            = outDir.resolve(configFilename)
      Files.copy(src, dst, StandardCopyOption.REPLACE_EXISTING)
      logger.info(f"copied default matsim config file to $dst")
    }

    configCopyProgram
  }

  def batchShellScript(
    confs: Seq[Configuration]
  ): String = {
    val filenames = confs.map { c => s""""${c.filename}"""" }.mkString(" ")
    s"""#!/bin/bash
       |
       |declare -a files=($filenames)
       |
       |for file in "$${files[@]}"
       |do
       |  echo -e "java exec: $$1"
       |  echo -e "java jar:  $$2"
       |  echo -e "running $$file"
       |  $$1 -Xmx10G -jar $$2 -c "$$file"
       |done
       |""".stripMargin
  }

  val LeadingEnumerationRegex: Regex = "(\\d+)-.*".r

  /**
    * hackish solution to get the number of the experiment directly from the conf filename
    * used to create the experiment logging directory.
    *
    * @param file the configuration file name
    * @return just whatever number starts the filename (any number of contiguous digits)
    */
  def leadingEnumerationFrom(file: File): String = {
    val fileName = file.getName
    fileName match {
      case LeadingEnumerationRegex(enumeration) => enumeration
      case other =>
        throw new IllegalArgumentException(s"input file name must have a leading enumeration, but found $other")
    }
  }
}

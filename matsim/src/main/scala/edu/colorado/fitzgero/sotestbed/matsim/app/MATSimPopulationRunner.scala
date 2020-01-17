package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io
import java.io.File
import java.time.LocalTime

import scala.util.Try
import scala.xml.XML
import scala.xml.dtd.{DocType, SystemID}

import com.typesafe.config.ConfigFactory
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimPopConfig
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimPopConfig.PopSampling.PopSamplingFailure
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population.UniformPopSamplingAlgorithm
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.Agent
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import org.matsim.core.network.NetworkUtils
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import MATSimPopConfig.localDateConvert

object MATSimPopulationRunner {

  def generateUniformPopulation(
    matsimNetworkFile: File,
    popFileDestination: File,
    popSize: Int,
    adoptionRate: Double,
    workActivityMinTime: LocalTime = LocalTime.parse("08:30:00"),
    workActivityMaxTime: LocalTime = LocalTime.parse("09:30:00"),
    workDurationHours: Int = 8,
    seed: Option[Long] = None
  ): Either[PopSamplingFailure, Unit] = {
    val result: Either[io.Serializable, UniformPopSamplingAlgorithm] = for {
      roadNetwork <- LocalAdjacencyListFlowNetwork.fromMATSimXML(matsimNetworkFile)
      matsimNetwork <- Try{ NetworkUtils.readNetwork(matsimNetworkFile.toString) }.toEither
    } yield {
      UniformPopSamplingAlgorithm(
        roadNetwork,
        matsimNetwork,
        popSize,
        adoptionRate,
        workActivityMinTime,
        workActivityMaxTime,
        workDurationHours,
        seed
      )
    }

    result.left.map { s =>
      PopSamplingFailure.BuildPopSamplingAlgorithmFailure(s.toString)
    } match {
      case Left(e) => Left(e)
      case Right(alg) =>
        val population: List[Agent] = alg.generate
        // converts each agent to xml, announcing any errors along the way
        val (agents, failures) = population.foldLeft((List.empty[xml.Elem], 0)){ (acc, agent) =>

          agent.toXML match {
            case Right(a) =>
              (a +: acc._1, acc._2)
            case Left(e) =>
              println(e)
              (acc._1, acc._2 + 1)
          }
        }

        if (failures > 0) {
          println(s"$failures agent generation failures resulting in population of size ${agents.length}")
        }

        // write new population to disk
        XML.save(
          popFileDestination.toString,
          <population>
            {agents}
          </population>,
          "UTF8",
          xmlDecl = true,
          DocType("population", SystemID("matsim/src/main/resources/matsim-dtd/population_v6.dtd"), Nil)
        )
        Right()
    }
  }

  def generatePopulationFromConfig(
    populationConfig: File,
    seed: Long): Unit = {
    for {
      config <- ConfigSource.fromConfig(ConfigFactory.parseFile(populationConfig)).load[MATSimPopConfig]
      configWithSeed = config.updateSeed(seed)
      popSamplingAlgorithm <- configWithSeed.pop.popSampling.build(configWithSeed)
      population = popSamplingAlgorithm.generate
    } yield {

      // converts each agent to xml, announcing any errors along the way
      val (agents, failures) = population.foldLeft((List.empty[xml.Elem], 0)){ (acc, agent) =>

        agent.toXML match {
          case Right(a) =>
            (a +: acc._1, acc._2)
          case Left(e) =>
            println(e)
            (acc._1, acc._2 + 1)
        }
      }

      if (failures > 0) {
        println(s"$failures agent generation failures resulting in population of size ${agents.length}")
      }

      // write new population to disk
      XML.save(
        configWithSeed.fs.populationFileDestination.toString,
        <population>
          {agents}
        </population>,
        "UTF8",
        xmlDecl = true,
        DocType("population", SystemID("matsim/src/main/resources/matsim-dtd/population_v6.dtd"), Nil)
      )
    }
  }
}

package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io
import java.io.File
import java.time.LocalTime

import scala.util.Try
import scala.xml.XML
import scala.xml.dtd.{DocType, SystemID}

import com.typesafe.config.ConfigFactory
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimPopConfig
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population.{
  PopSamplingAlgorithm,
  UniformEdgePopulationSamplingAlgorithm,
  UniformPolygonPopulationSamplingAlgorithm
}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.Agent
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import org.matsim.core.network.NetworkUtils
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import MATSimPopConfig.localDateConvert
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population.UniformEdgePopSamplingSingleTrip
import com.typesafe.scalalogging.LazyLogging

object MATSimPopulationRunner extends LazyLogging {

  sealed trait UniformPopAlgType

  object UniformPopAlgType {
    case object LinkBased    extends UniformPopAlgType
    case object PolygonBased extends UniformPopAlgType
  }

  // TODO: feed MATSimRunConfig and use to unpack the PopulationAlgorithm, ditch UniformPopAlgType
  def generateUniformPopulation(
    networkFile: File,
    polygonFileOption: Option[File],
    popFileDestination: File,
    popSize: Int,
    adoptionRate: Double,
    uniformPopAlgType: UniformPopAlgType = UniformPopAlgType.PolygonBased,
    workActivityMinTime: LocalTime = LocalTime.parse("08:30:00"),
    workActivityMaxTime: LocalTime = LocalTime.parse("09:30:00"),
    workDurationHours: Int = 8,
    seed: Option[Long] = None,
    geometrySourceIsGeoJson: Boolean = true
  ): Either[Error, Unit] = {
    val result: Either[Throwable, PopSamplingAlgorithm] =
      polygonFileOption match {
        case None =>
          for {
            roadNetwork   <- LocalAdjacencyListFlowNetwork.fromMATSimXML(networkFile).left.map { new Error(_) }
            matsimNetwork <- Try { NetworkUtils.readNetwork(networkFile.toString) }.toEither
          } yield {
            // rjf 2022-08-06 let's break each "person" into two, each with one trip
            UniformEdgePopSamplingSingleTrip(
              roadNetwork,
              matsimNetwork,
              popSize,
//              adoptionRate,
              workActivityMinTime,
              workActivityMaxTime,
              workDurationHours,
              seed
            )
          }
        case Some(polygonFile) =>
          val readGeometryResult = if (geometrySourceIsGeoJson) {
            logger.info(s"reading GeoJson boundary")
            PopulationSamplingOps.readBoundingGeometryGeoJson(polygonFile)
          } else {
            logger.info(s"reading csv boundary")
            PopulationSamplingOps.readBoundingGeometryXYCsv(polygonFile)
          }
          for {
            geometry      <- readGeometryResult
            matsimNetwork <- Try { NetworkUtils.readNetwork(networkFile.toString) }.toEither
          } yield {
            UniformPolygonPopulationSamplingAlgorithm(
              geometry,
              boundingGeometrySRID = 4326, // assumed to be LAT LON
              networkSRID = 3857,          // assumed to be web mercator
              matsimNetwork,
              popSize,
//              adoptionRate,
              workActivityMinTime,
              workActivityMaxTime,
              workDurationHours,
              seed
            )
          }
      }

    val x = for {
      alg        <- result
      population <- alg.generate
    } yield {
      // converts each agent to xml, announcing any errors along the way
      val (agents, failures) = population.foldLeft((List.empty[xml.Elem], 0)) { (acc, agent) =>
        agent.toXML match {
          case Right(a) =>
            (a +: acc._1, acc._2)
          case Left(e) =>
            println(e)
            (acc._1, acc._2 + 1)
        }
      }

      if (failures > 0) {
        Left(new Error(s"$failures agent generation failures resulting in population of size ${agents.length}"))
      } else {
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
        Right(())
      }
    }

    x.flatten.left.map { t => new Error(t) }

    // result.left.map { s => new Error(s.toString) } match {
    //   case Left(e) => Left(e)
    //   case Right(alg) =>
    //     val population: List[Agent] = alg.generate
    //     // converts each agent to xml, announcing any errors along the way
    //     val (agents, failures) = population.foldLeft((List.empty[xml.Elem], 0)) { (acc, agent) =>
    //       agent.toXML match {
    //         case Right(a) =>
    //           (a +: acc._1, acc._2)
    //         case Left(e) =>
    //           println(e)
    //           (acc._1, acc._2 + 1)
    //       }
    //     }

    //     if (failures > 0) {
    //       println(s"$failures agent generation failures resulting in population of size ${agents.length}")
    //     }

    //     // write new population to disk
    //     XML.save(
    //       popFileDestination.toString,
    //       <population>
    //         {agents}
    //       </population>,
    //       "UTF8",
    //       xmlDecl = true,
    //       DocType("population", SystemID("matsim/src/main/resources/matsim-dtd/population_v6.dtd"), Nil)
    //     )
    //     Right()
    // }
  }

  def generatePopulationFromConfig(populationConfig: File, seed: Long): Unit = {
    for {
      config <- ConfigSource.fromConfig(ConfigFactory.parseFile(populationConfig)).load[MATSimPopConfig]
      configWithSeed = config.updateSeed(seed)
      popSamplingAlgorithm <- configWithSeed.pop.popSampling.build(configWithSeed)
      population           <- popSamplingAlgorithm.generate
    } yield {

      // converts each agent to xml, announcing any errors along the way
      val (agents, failures) = population.foldLeft((List.empty[xml.Elem], 0)) { (acc, agent) =>
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

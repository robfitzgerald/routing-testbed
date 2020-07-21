package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig

import java.io
import java.io.File
import java.nio.file.Path
import java.time.LocalTime

import scala.util.Try

import edu.colorado.fitzgero.sotestbed.matsim.app.PopulationSamplingOps
import pureconfig._
import pureconfig.configurable._
import pureconfig.ConvertHelpers._
import pureconfig.generic.auto._
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population.{
  PopSamplingAlgorithm,
  UniformEdgePopulationSamplingAlgorithm,
  UniformPolygonPopulationSamplingAlgorithm
}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.AgentActivity
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import org.matsim.core.network.NetworkUtils

case class MATSimPopConfig(
  fs: MATSimPopConfig.Fs,
  pop: MATSimPopConfig.Pop
) {

  def updateSeed(newSeed: Long): MATSimPopConfig = {
    pop.popSampling match {
      case u: MATSimPopConfig.PopSampling.UniformPopLinkSampling =>
        val updatedPopSampling: MATSimPopConfig.PopSampling = u.copy(seed = Some { newSeed })
        val newPop: MATSimPopConfig.Pop = this.pop.copy(
          popSampling = updatedPopSampling
        )
        this.copy(pop = newPop)
      case u: MATSimPopConfig.PopSampling.UniformPopPolygonSampling =>
        val updatedPopSampling: MATSimPopConfig.PopSampling = u.copy(seed = Some { newSeed })
        val newPop: MATSimPopConfig.Pop = this.pop.copy(
          popSampling = updatedPopSampling
        )
        this.copy(pop = newPop)
    }
  }
}

object MATSimPopConfig {

  implicit val localDateConvert: ConfigConvert[LocalTime] = localTimeConfigConvert(AgentActivity.MATSimTextTimeFormat)

  final case class Fs(
    matsimNetworkFile: File,
    populationFileDestination: File,
    name: String
  )

  final case class Pop(
    size: Int,
    adoptionRate: Double,
    popSampling: PopSampling
  )

  sealed trait PopSampling {
    def build(MATSimPopConfig: MATSimPopConfig): Either[PopSampling.PopSamplingFailure, PopSamplingAlgorithm]
  }

  object PopSampling {

    /**
      * build a population using uniform sampling over the graph edge set and using
      * a time range for the workplace activity
      * @param workActivityMinTime
      * @param workActivityMaxTime
      * @param workDurationHours
      * @param seed
      */
    final case class UniformPopLinkSampling(
      workActivityMinTime: LocalTime = LocalTime.parse("08:30:00"),
      workActivityMaxTime: LocalTime = LocalTime.parse("09:30:00"),
      workDurationHours: Int = 8,
      seed: Option[Long] = None,
    ) extends PopSampling {

      def build(matsimPopConfig: MATSimPopConfig): Either[PopSamplingFailure, PopSamplingAlgorithm] = {
        val result: Either[io.Serializable, UniformEdgePopulationSamplingAlgorithm] = for {
          roadNetwork   <- LocalAdjacencyListFlowNetwork.fromMATSimXML(matsimPopConfig.fs.matsimNetworkFile)
          matsimNetwork <- Try { NetworkUtils.readNetwork(matsimPopConfig.fs.matsimNetworkFile.toString) }.toEither
        } yield {
          UniformEdgePopulationSamplingAlgorithm(
            roadNetwork,
            matsimNetwork,
            matsimPopConfig.pop.size,
//            matsimPopConfig.pop.adoptionRate,
            workActivityMinTime,
            workActivityMaxTime,
            workDurationHours,
            seed
          )
        }

        result.left.map { s =>
          PopSamplingFailure.BuildPopSamplingAlgorithmFailure(s.toString)
        }
      }
    }

    /**
      *
      * @param geometryPath
      * @param geometrySRID
      * @param workActivityMinTime
      * @param workActivityMaxTime
      * @param workDurationHours
      * @param seed
      */
    final case class UniformPopPolygonSampling(
      geometryPath: File,
      geometrySRID: Int = 4326, // assumed to be WGS84
      networkSRID: Int = 3857, // assumed to be web mercator
      workActivityMinTime: LocalTime = LocalTime.parse("08:30:00"),
      workActivityMaxTime: LocalTime = LocalTime.parse("09:30:00"),
      workDurationHours: Int = 8,
      seed: Option[Long] = None,
    ) extends PopSampling {

      def build(matsimPopConfig: MATSimPopConfig): Either[PopSamplingFailure, PopSamplingAlgorithm] = {
        val result: Either[io.Serializable, UniformPolygonPopulationSamplingAlgorithm] = for {
          geometry      <- PopulationSamplingOps.readBoundingGeometryFile(geometryPath)
          matsimNetwork <- Try { NetworkUtils.readNetwork(matsimPopConfig.fs.matsimNetworkFile.toString) }.toEither
        } yield {
          UniformPolygonPopulationSamplingAlgorithm(
            geometry,
            boundingGeometrySRID = geometrySRID,
            networkSRID = networkSRID,
            matsimNetwork,
            matsimPopConfig.pop.size,
//            matsimPopConfig.pop.adoptionRate,
            workActivityMinTime,
            workActivityMaxTime,
            workDurationHours,
            seed
          )
        }

        result.left.map { s =>
          PopSamplingFailure.BuildPopSamplingAlgorithmFailure(s.toString)
        }
      }
    }

    sealed trait PopSamplingFailure

    object PopSamplingFailure {
      final case class BuildPopSamplingAlgorithmFailure(msg: String) extends PopSamplingFailure
    }
  }
}

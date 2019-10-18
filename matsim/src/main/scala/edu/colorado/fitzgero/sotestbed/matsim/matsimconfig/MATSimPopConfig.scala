package edu.colorado.fitzgero.sotestbed.matsim.matsimconfig

import java.io.File
import java.time.LocalTime

import com.typesafe.config.ConfigFactory
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import pureconfig._
import pureconfig.configurable._
import pureconfig.ConvertHelpers._
import pureconfig.generic.auto._
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.population.{PopSamplingAlgorithm, UniformPopSamplingAlgorithm}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.AgentActivity
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork

case class MATSimPopConfig(
  fs: MATSimPopConfig.Fs,
  pop: MATSimPopConfig.Pop
)

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
    final case class UniformPopSampling(
      workActivityMinTime: LocalTime = LocalTime.parse("08:30:00"),
      workActivityMaxTime: LocalTime = LocalTime.parse("09:30:00"),
      workDurationHours: Int = 8,
      seed: Option[Long] = None,
    ) extends PopSampling {

      def build(matsimPopConfig: MATSimPopConfig): Either[PopSamplingFailure, PopSamplingAlgorithm] = {
        val result = for {
          roadNetwork <- LocalAdjacencyListFlowNetwork.fromMATSimXML(matsimPopConfig.fs.matsimNetworkFile)
        } yield {
          UniformPopSamplingAlgorithm(
            roadNetwork,
            matsimPopConfig.pop.size,
            matsimPopConfig.pop.adoptionRate,
            workActivityMinTime,
            workActivityMaxTime,
            workDurationHours,
            seed
          )
        }

        result.left.map { s =>
          PopSamplingFailure.BuildPopSamplingAlgorithmFailure(s)
        }
      }
    }

    sealed trait PopSamplingFailure

    object PopSamplingFailure {
      final case class BuildPopSamplingAlgorithmFailure(msg: String) extends PopSamplingFailure
    }
  }
}

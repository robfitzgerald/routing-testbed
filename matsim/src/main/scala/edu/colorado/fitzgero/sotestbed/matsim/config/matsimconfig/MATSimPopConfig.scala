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
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population.UniformEdgePopSamplingSingleTrip

case class MATSimPopConfig(
  fs: MATSimPopConfig.Fs,
  pop: MATSimPopConfig.Pop
) {

  def updateSeed(newSeed: Long): MATSimPopConfig = {
    // scala Random takes Int, silly
    val updatedSampling = pop.popSampling match {
      case u: PopSampling.UniformPopLinkSampling    => u.copy(seed = Some(newSeed))
      case u: PopSampling.UniformPopPolygonSampling => u.copy(seed = Some(newSeed))
      case u: PopSampling.DemandSamplingTableInput  => u.copy(seed = Some(newSeed.toInt))
    }
    this.copy(pop = this.pop.copy(popSampling = updatedSampling))
  }
}

object MATSimPopConfig {

  implicit val localDateConvert: ConfigConvert[LocalTime] = localTimeConfigConvert(AgentActivity.MATSimTextTimeFormat)

  final case class Fs(
    matsimNetworkFile: File,
    populationFileDestination: File,
    name: String
  )

  final case class Pop(size: Int, adoptionRate: Double, popSampling: PopSampling)
}

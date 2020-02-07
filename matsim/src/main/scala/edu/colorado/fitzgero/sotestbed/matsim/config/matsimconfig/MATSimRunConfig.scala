package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig

import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.population.Person

final case class MATSimRunConfig(
  pop: MATSimRunConfig.PopulationData,
  io: MATSimConfig.IO,
  routing: MATSimConfig.Routing,
  run: MATSimConfig.Run,
  algorithm: MATSimConfig.Algorithm
)

object MATSimRunConfig {

  def apply(agentsUnderControl: Set[Id[Person]], config: MATSimConfig): MATSimRunConfig =
    MATSimRunConfig(
      pop = PopulationData(agentsUnderControl),
      io = config.io,
      routing = config.routing,
      run = config.run,
      algorithm = config.algorithm
    )

  final case class PopulationData(
    agentsUnderControl: Set[Id[Person]],
  )
}

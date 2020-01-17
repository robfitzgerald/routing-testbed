package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig

import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.population.Person

final case class MATSimRunConfig(
  pop: MATSimRunConfig.Population,
  io: MATSimConfig.IO,
  routing: MATSimConfig.Routing,
  run: MATSimConfig.Run,
  algorithm: MATSimConfig.Algorithm
)

object MATSimRunConfig {

  def apply(agentsUnderControl: Set[Id[Person]], config: MATSimConfig): MATSimRunConfig =
    MATSimRunConfig(
      pop = Population(agentsUnderControl),
      io = config.io,
      routing = config.routing,
      run = config.run,
      algorithm = config.algorithm
    )

  final case class Population(
    agentsUnderControl: Set[Id[Person]],
  )
}

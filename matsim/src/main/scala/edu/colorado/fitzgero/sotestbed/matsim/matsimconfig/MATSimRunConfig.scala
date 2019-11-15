package edu.colorado.fitzgero.sotestbed.matsim.matsimconfig

import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.population.Person

final case class MATSimRunConfig (
  pop: MATSimRunConfig.Population,
  io : MATSimConfig.IO,
  routing: MATSimConfig.Routing,
  run: MATSimConfig.Run
)

object MATSimRunConfig {

  final case class Population(
    agentsUnderControl: Set[Id[Person]],
  )

}
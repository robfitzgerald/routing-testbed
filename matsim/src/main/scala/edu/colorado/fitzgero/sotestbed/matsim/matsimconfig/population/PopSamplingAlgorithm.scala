package edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.population

import edu.colorado.fitzgero.sotestbed.matsim.model.agent.Agent

trait PopSamplingAlgorithm {
  def generate: List[Agent]
}

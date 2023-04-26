package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population

import edu.colorado.fitzgero.sotestbed.matsim.model.agent.Agent

trait PopSamplingAlgorithm {
  def generate: Either[Error, List[Agent]]
}

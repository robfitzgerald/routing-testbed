package edu.colorado.fitzgero.sotestbed.matsim.matsimconfig

import java.nio.file.{Path, Paths}

import edu.colorado.fitzgero.sotestbed.matsim.MATSimProxy
import edu.colorado.fitzgero.sotestbed.model.numeric.{SimTime, TravelTimeSeconds}
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.population.Person

final case class MATSimConfig(
    fs: MATSimConfig.FileSystem,
    run: MATSimConfig.Run,
    routing: MATSimConfig.Routing
)

object MATSimConfig {

  final case class Run(
      iterations      : Int = 0,
      endOfRoutingTime: SimTime
  )

  final case class Routing(
      agentsUnderControl: Set[Id[Person]],
      networkFlowCaptureBuffer: SimTime,
      batchWindow: SimTime,
      reasonableReplanningLeadTime: TravelTimeSeconds,
      routeIterations: List[Int]
  )

  final case class FileSystem(
      name: String = "matsim",
      workingDirectory: Path = Paths.get("/tmp"),
  ) {
    def experimentSubdirectoryName: String = s"$name-${System.currentTimeMillis}"
  }
}

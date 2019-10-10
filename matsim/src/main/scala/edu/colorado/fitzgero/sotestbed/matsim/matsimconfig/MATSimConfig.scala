package edu.colorado.fitzgero.sotestbed.matsim.matsimconfig

import java.io.File
import java.nio.file.{Path, Paths}

import edu.colorado.fitzgero.sotestbed.model.numeric.{NaturalNumber, SimTime, TravelTimeSeconds}
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.population.Person

final case class MATSimConfig(
  fs: MATSimConfig.FileSystem,
  run: MATSimConfig.Run,
  routing: MATSimConfig.Routing
)

object MATSimConfig {

  final case class Run(
    iterations: Int = 0,
    startOfSimTime: SimTime,
    endOfSimTime: SimTime,
    endOfRoutingTime: SimTime
  )

  final case class Routing(
    agentsUnderControl: Set[Id[Person]],
    networkFlowCaptureBuffer: SimTime,
    k: NaturalNumber,
    batchWindow: SimTime,
    reasonableReplanningLeadTime: TravelTimeSeconds,
    routeIterations: List[Int]
  )

  final case class FileSystem(
    matsimNetworkFile: File,
    workingDirectory: Path = Paths.get("/tmp"),
    name: String = "so-matsim"
  ) {

    def experimentSubdirectoryName: String =
      s"$name-${System.currentTimeMillis}"
  }
}

package edu.colorado.fitzgero.sotestbed.matsim.matsimconfig

import java.io.File
import java.nio.file.{Path, Paths}

import scala.concurrent.duration.Duration

import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, NaturalNumber, SimTime, TravelTimeSeconds}

final case class MATSimConfig(
  fs: MATSimConfig.FileSystem,
  run: MATSimConfig.Run,
  routing: MATSimConfig.Routing,
  algorithm: MATSimConfig.Algorithm
)

object MATSimConfig {

  final case class Run(
    iterations: Int,
    soRoutingIterationCycle: Int,
    startOfSimTime: SimTime,
    endOfSimTime: SimTime,
    endOfRoutingTime: SimTime,
    matsimStepSize: SimTime,
    simulationTailTimeout: Duration
  ) {
    require(soRoutingIterationCycle < iterations, "matsimConfig.run.soRoutingIterationCycle needs to be less than matsimConfig.run.iterations")
  }

  final case class Routing(
    networkFlowCaptureBuffer: SimTime,
    k: NaturalNumber,
    batchWindow: SimTime,
    reasonableReplanningLeadTime: TravelTimeSeconds,
    theta: Cost
  )

  final case class FileSystem(
    matsimNetworkFile: File,
    populationFile: File,
    matsimConfigFile: File,
    experimentTimestamp: Long = System.currentTimeMillis,
    workingBaseDirectory: Path = Paths.get("/tmp"),
    name: String = "so-matsim"
  ) {

    def workingDirectory: Path = this.workingBaseDirectory.resolve(s"$experimentTimestamp")

    def experimentSubdirectoryName: String =
      s"$name-${System.currentTimeMillis}"
  }

  final case class Algorithm(

  )
}

package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig

import java.nio.file.Path

import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.population.Person

final case class MATSimRunConfig(
  io: MATSimConfig.IO,
  scenarioData: MATSimRunConfig.ScenarioData,
  routing: MATSimConfig.Routing,
  run: MATSimConfig.Run,
  algorithm: MATSimConfig.Algorithm,
  agentsUnderControl: Set[Id[Person]] = Set.empty
) {

  def experimentLoggingDirectory: Path = {
    scenarioData.toTrialPath(io.outputBaseDirectory, io.batchName)
  }

  def experimentDirectory: Path = {
    scenarioData.toExperimentPath(io.outputBaseDirectory, io.batchName)
  }
}

object MATSimRunConfig {

  def apply(config: MATSimConfig, scenarioData: ScenarioData): MATSimRunConfig =
    MATSimRunConfig(
      io = config.io,
      scenarioData = scenarioData,
      routing = config.routing,
      run = config.run,
      algorithm = config.algorithm
    )

//  def apply(agentsUnderControl: Set[Id[Person]], config: MATSimConfig, scenarioData: ScenarioData): MATSimRunConfig =
//    MATSimRunConfig(
////      pop = PopulationData(agentsUnderControl),
//      io = config.io,
//      scenarioData = scenarioData,
//      routing = config.routing,
//      run = config.run,
//      algorithm = config.algorithm
//    )

  final case class ScenarioData(
    algorithm: String,
    trialNumber: Int,
    popSize: Int,
    variationName: String,
    headerColumnOrder: List[String],
    scenarioParameters: Map[String, String]
  ) {

    // prints this scenario's run parameters as csv row entries in the correct order
    def toCSVRow: String = algorithm match {
      case "selfish" => s"selfish-$popSize-trial=$trialNumber"
      case _         => s"$variationName"
    }
    //        headerColumnOrder
    //          .map{ colName => this.scenarioParameters.getOrElse(colName, "") }
    //          .mkString(",")

    def toVariationPath(basePath: Path, batchName: String): Path =
      algorithm match {
        case "selfish" =>
          basePath.resolve(batchName).resolve(s"selfish").resolve(s"$popSize-${trialNumber.toString}-logging")
        case _ =>
          basePath.resolve(batchName).resolve(variationName)
      }

    def toTrialPath(basePath: Path, batchName: String): Path =
      algorithm match {
        case "selfish" =>
          basePath.resolve(batchName).resolve(s"selfish").resolve(s"$popSize-${trialNumber.toString}-logging")
        case _ =>
          basePath.resolve(batchName).resolve(variationName).resolve(trialNumber.toString)
      }

    def toExperimentPath(basePath: Path, batchName: String): Path =
      algorithm match {
        case "selfish" =>
          basePath.resolve(batchName).resolve(s"selfish").resolve(s"$popSize-${trialNumber.toString}")
        case _ =>
          basePath.resolve(batchName).resolve(variationName).resolve(trialNumber.toString).resolve(algorithm)
      }

  }

//  final case class PopulationData(
//    agentsUnderControl: Set[Id[Person]],
//  )
}

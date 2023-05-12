package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig

import java.nio.file.Path

import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.population.Person

final case class MATSimRunConfig(
  underlying: MATSimConfig,
  trial: Option[Int] = None,
  matsimRunSeed: Option[Int] = None,
  agentsUnderControl: Set[Id[Person]] = Set.empty
  // scenarioData: MATSimRunConfig.ScenarioData,
) {

  // convenience methods so MATSimRunConfig shares same API as MATSimConfig
  def io         = this.underlying.io
  def routing    = this.underlying.routing
  def run        = this.underlying.run
  def algorithm  = this.underlying.algorithm
  def population = this.underlying.population

  def outputBaseDirectory: Path        = this.underlying.io.outputBaseDirectory
  def trialSharedDirectory: Path       = this.outputBaseDirectory.resolve(this.underlying.io.batchName)
  def experimentLoggingDirectory: Path = this.trialSharedDirectory.resolve(this.underlying.algorithm.name)
  def experimentMATSimDirectory: Path  = this.experimentLoggingDirectory.resolve("matsim")
  def populationFilename: String       = this.underlying.population.filename(this.trial)
  def populationFilepath: Path         = this.trialSharedDirectory.resolve(populationFilename)

}

// object MATSimRunConfig {

//   def apply(config: MATSimConfig, trialNumber: Option[Int] = None): MATSimRunConfig =
//     MATSimRunConfig(
//       underlying = config,
//       trial = trialNumber
//       // io = config.io,
//       // scenarioData = scenarioData,
//       // routing = config.routing,
//       // run = config.run,
//       // algorithm = config.algorithm,
//       // population = config.population
//     )

// }

//  def apply(agentsUnderControl: Set[Id[Person]], config: MATSimConfig, scenarioData: ScenarioData): MATSimRunConfig =
//    MATSimRunConfig(
////      pop = PopulationData(agentsUnderControl),
//      io = config.io,
//      scenarioData = scenarioData,
//      routing = config.routing,
//      run = config.run,
//      algorithm = config.algorithm
//    )

//   final case class ScenarioData(
//     algorithm: String,
//     trialNumber: Int,
//     popSize: Int,
//     variationName: String,
//     headerColumnOrder: List[String],
//     scenarioParameters: Map[String, String]
//   ) {

//     // prints this scenario's run parameters as csv row entries in the correct order
//     def toCSVRow: String = algorithm match {
//       case "selfish" => s"selfish-$popSize-trial=$trialNumber"
//       case _         => s"$variationName"
//     }
//     //        headerColumnOrder
//     //          .map{ colName => this.scenarioParameters.getOrElse(colName, "") }
//     //          .mkString(",")

// //    def toTrialPath(basePath: Path, batchName: String): Path =
// //      algorithm match {
// //        case "selfish" =>
// //          basePath.resolve(batchName).resolve(s"selfish").resolve(popSize.toString).resolve(trialNumber.toString)
// //        case _ =>
// //          basePath.resolve(batchName).resolve(variationName).resolve(trialNumber.toString)
// //      }

//     /**
//       * construct the path to the
//       * @param basePath base output directory
//       * @param batchName something shared across different algorithms sharing the same population resource,
//       *                  such as an enumeration
//       * @return outer experiment directory (not the matsim directory), where we drop our logging
//       */
//     def loggingDirectory(basePath: Path, batchName: String): Path =
//       basePath.resolve(batchName).resolve(algorithm)

//     /**
//       * directory to write MATSim files
//       * @param basePath base output directory
//       * @param batchName something shared across different algorithms sharing the same population resource,
//       *                  such as an enumeration
//       * @return
//       */
//     def matsimDirectory(basePath: Path, batchName: String): Path =
//       basePath.resolve(batchName).resolve(algorithm).resolve("matsim")

// //    def toExperimentPath(basePath: Path, batchName: String): Path =
// //      algorithm match {
// //        case "selfish" =>
// //          basePath
// //            .resolve(batchName)
// //            .resolve(s"selfish")
// //            .resolve(popSize.toString)
// //            .resolve(trialNumber.toString)
// //            .resolve("matsim")
// //        case _ =>
// //          basePath.resolve(batchName).resolve(variationName).resolve(trialNumber.toString).resolve(algorithm)
// //      }

// }
// }

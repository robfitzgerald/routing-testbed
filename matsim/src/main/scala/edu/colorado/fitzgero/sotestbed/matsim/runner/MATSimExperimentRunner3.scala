package edu.colorado.fitzgero.sotestbed.matsim.runner

import java.io.File
import java.nio.file.Files

import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{RoutingAlgorithm, RoutingAlgorithm2, SelfishSyncRoutingBPR}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.algorithm.selection.rl.RLSelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.{SelectionAlgorithm, SelectionRunner}
import edu.colorado.fitzgero.sotestbed.config.{RoutingReportConfig, SelectionAlgorithmConfig}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig.Algorithm
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.experiment.LocalMATSimRoutingExperiment2
import edu.colorado.fitzgero.sotestbed.matsim.io.population.Bank
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.PopulationOps
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.reports.RoutingReports
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.population.Person

//import kantan.csv._
//import kantan.csv.ops._

case class MATSimExperimentRunner3(matsimRunConfig: MATSimRunConfig, seed: Long) extends LazyLogging {

  /**
    * performs a synchronous run of a MATSim simulation from a MATSimConfig
    *
    * @return the effect of running this experiment
    */
  def run(): IO[Unit] = {

    val result = for {
      network <- LocalAdjacencyListFlowNetwork
        .fromMATSimXML(
          matsimRunConfig.io.matsimNetworkFile,
          matsimRunConfig.routing.batchWindow
        )
        .left
        .map { s => new Error(s) }
      agentsUnderControlPercentage = if (matsimRunConfig.algorithm.isInstanceOf[MATSimConfig.Algorithm.Selfish]) 0.0
      else matsimRunConfig.routing.adoptionRate
      agentsUnderControl <- matsimRunConfig.algorithm match {
        case _: Algorithm.Selfish => Right(Set.empty[Id[Person]])
        case so: Algorithm.SystemOptimal =>
          so.selectionAlgorithm match {
            case rl: SelectionAlgorithmConfig.RLSelection =>
              PopulationOps.readGrouping(rl.groupingFile)
            case _ =>
              PopulationOps.loadAgentsUnderControl(
                matsimRunConfig.io.populationFile,
                agentsUnderControlPercentage
              )
          }
      }
      config = matsimRunConfig.copy(agentsUnderControl = agentsUnderControl)
    } yield {

      Files.createDirectories(config.experimentDirectory)

      // build some cost functions
      val freeFlowCostFunction: EdgeBPR => Cost = (edgeBPR: EdgeBPR) => edgeBPR.freeFlowCost
      val costFunction: EdgeBPR => Cost = {
        val marginalCostFn: EdgeBPR => Flow => Cost = config.algorithm.marginalCostFunction.build()
        edgeBPR: EdgeBPR => marginalCostFn(edgeBPR)(Flow.Zero)
      }

      // how we read reports
      val routingReporter: RoutingReports[IO, Coordinate, EdgeBPR] =
        config.io.routingReportConfig match {
          case RoutingReportConfig.Inactive =>
            RoutingReportConfig.Inactive.build()
          case RoutingReportConfig.AggregateData =>
            RoutingReportConfig.AggregateData.build(config.experimentLoggingDirectory, costFunction)
          case RoutingReportConfig.Batch =>
            RoutingReportConfig.Batch.build(config.experimentLoggingDirectory)
          case RoutingReportConfig.CompletePath =>
            RoutingReportConfig.CompletePath.build(config.experimentLoggingDirectory, costFunction)
          case RoutingReportConfig.Heatmap =>
            RoutingReportConfig.Heatmap.build(
              config.experimentLoggingDirectory,
              SimTime.minute(config.io.heatmapLogCycleMinutes),
              config.io.heatmapH3Resolution,
              network,
              costFunction
            )
          case RoutingReportConfig.AllAggregate =>
            RoutingReportConfig.AllAggregate.build(
              config.experimentLoggingDirectory,
              SimTime.minute(config.io.heatmapLogCycleMinutes),
              config.io.heatmapH3Resolution,
              network,
              costFunction
            )
          case RoutingReportConfig.AllReporting =>
            RoutingReportConfig.AllReporting.build(
              config.experimentLoggingDirectory,
              SimTime.minute(config.io.heatmapLogCycleMinutes),
              config.io.heatmapH3Resolution,
              network,
              costFunction
            )
        }

      // the actual Simulation runner instance
      val experiment = new LocalMATSimRoutingExperiment2(
        new File(config.experimentLoggingDirectory.resolve(s"final-${config.algorithm.name}.log").toString),
        routingReporter
      )

      val ueRoutingAlgorithm: Option[RoutingAlgorithm[IO, Coordinate, EdgeBPR]] =
        config.routing.selfish match {
          case _: MATSimConfig.Routing.Selfish.Matsim =>
            None
          case MATSimConfig.Routing.Selfish
                .Dijkstra(pathToMarginalFlowsFunction, combineFlowsFunction, marginalCostFunction) =>
            Some {
              SelfishSyncRoutingBPR(
                marginalCostFunction.build(),
                pathToMarginalFlowsFunction.build(),
                combineFlowsFunction.build()
              )
            }
        }

      val soAssetsOrError: Either[Error, Option[(RoutingAlgorithm2, Map[String, Karma])]] =
        config.algorithm match {
          case so: Algorithm.SystemOptimal =>
            val soAlgorithmOrError = for {
              grid <- so.grid.build()
            } yield {
              val ksp: AltPathsAlgorithmRunner[IO, Coordinate, EdgeBPR] = {
                AltPathsAlgorithmRunner(
                  altPathsAlgorithm = so.kspAlgorithm.build(),
                  kspFilterFunction = so.kspFilterFunction.build(),
                  costFunction = costFunction,
                  freeFlowCostFunction = freeFlowCostFunction,
                  useFreeFlowNetworkCostsInPathSearch = so.useFreeFlowNetworkCostsInPathSearch,
                  seed = seed
                )
              }
              val selectionAlgorithm: SelectionAlgorithm[IO, Coordinate, EdgeBPR] =
                so.selectionAlgorithm.build(config.experimentLoggingDirectory)

              val sel: SelectionRunner[Coordinate] =
                SelectionRunner(
                  selectionAlgorithm = selectionAlgorithm,
                  pathToMarginalFlowsFunction = so.pathToMarginalFlowsFunction.build(),
                  combineFlowsFunction = so.combineFlowsFunction.build(),
                  marginalCostFunction = so.marginalCostFunction.build(),
                  minimumAverageImprovement = config.routing.minimumAverageImprovement
                )

              val bank: Map[String, Karma] = so.selectionAlgorithm match {
                case k: SelectionAlgorithmConfig.KarmaSelection =>
                  val agentStrings = agentsUnderControl.map { _.toString }
                  k.bankConfig.build(agentStrings)
                case _ =>
                  Map.empty
              }

//              // write initial bank to file system
//              val initBankFile = config.experimentLoggingDirectory.resolve("karma_bank_initial.csv").toFile
//              val writer       = initBankFile.asCsvWriter[(String, Long)](rfc.withHeader("agentId", "balance"))
//              writer.write(bank.toList.map { case (a, k) => (a, k.value) })

              val alg = RoutingAlgorithm2(
                altPathsAlgorithmRunner = ksp,
                batchingFunction = so.batchingFunction.build(grid),
                batchFilterFunction =
                  so.batchFilterFunction.build(Some(config.routing.minBatchSearchSpace), grid, costFunction),
                selectionRunner = sel,
                k = so.kspAlgorithm.k,
                minSearchSpaceSize = config.routing.minBatchSearchSpace
              )
              Some(alg, bank)
            }

            soAlgorithmOrError

          case _: Algorithm.Selfish =>
            Right(None)
        }

      val experimentIO: IO[experiment.ExperimentState] = for {
        soAssets <- IO.fromEither(soAssetsOrError)
        soRoutingAlgorithm = soAssets.map { case (alg, _) => alg }
        bank               = soAssets.map { case (_, bank) => bank }.getOrElse(Map.empty)
        experimentFinishState <- experiment.run(
          config = config,
          roadNetwork = network,
          ueRoutingAlgorithm = ueRoutingAlgorithm,
          soRoutingAlgorithm = soRoutingAlgorithm,
          bank = bank,
          updateFunction = config.algorithm.edgeUpdateFunction.build(),
          batchWindow = config.routing.batchWindow,
          minRequestUpdateThreshold = config.routing.minRequestUpdateThreshold
        )
      } yield experimentFinishState

      for {
        finalState <- experimentIO
      } yield {
        experiment.close()

        soAssetsOrError match {
          case Right(Some((ra, initialBank))) =>
            // report final bank balance

            val bankResult =
              Bank
                .writeFinalLedger(initialBank, finalState.bank, config.experimentLoggingDirectory)
                .map { bankFilePath => logger.info(f"bank file written to $bankFilePath") }

            // if there's an RL trainer with an episode started, let's end that episode
            ra.selectionRunner.selectionAlgorithm match {
              case rlsa: RLSelectionAlgorithm =>
                for {
                  _ <- bankResult
                  _ <- rlsa.reportAgentsAreDone()
                  _ <- rlsa.close()
                } yield ()
              case _ =>
                bankResult
            }
          case _ =>
            IO.pure()
        }
      }
    }

    result match {
      case Left(error) =>
        IO.raiseError(error)
      case Right(value) =>
        value.flatten
    }
  }
}

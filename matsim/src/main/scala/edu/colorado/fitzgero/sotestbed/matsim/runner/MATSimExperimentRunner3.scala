package edu.colorado.fitzgero.sotestbed.matsim.runner

import java.io.File
import java.nio.file.Files

import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{
  BatchedKspSelectionRouting,
  RoutingAlgorithm,
  SelfishSyncRoutingBPR
}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.algorithm.selection.rl.RLSelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.{SelectionAlgorithm, SelectionRunner}
import edu.colorado.fitzgero.sotestbed.config.{
  FreeFlowCostFunctionConfig,
  RoutingReportConfig,
  SelectionAlgorithmConfig
}
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
import edu.colorado.fitzgero.sotestbed.config.SelectionAlgorithmConfig._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.KarmaSelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.KarmaSelectionAlgorithm._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.DriverPolicy._
import edu.colorado.fitzgero.sotestbed.config.DriverPolicyConfig._
import edu.colorado.fitzgero.sotestbed.rllib._
import cats.implicits._
import scala.util.Try
import edu.colorado.fitzgero.sotestbed.config.DriverPolicyConfig
import edu.colorado.fitzgero.sotestbed.config.DriverPolicyConfig
import edu.colorado.fitzgero.sotestbed.config.DriverPolicyConfig
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.DriverPolicyStructure.MultiAgentPolicy
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.DriverPolicyStructure.SingleAgentPolicy
import edu.colorado.fitzgero.sotestbed.algorithm.grid.CoordinateGrid2PrintOps
import edu.colorado.fitzgero.sotestbed.algorithm.routing.UserOptimalAuctionSelection
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig.Algorithm.Selfish
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig.Algorithm.SystemOptimal
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig.Algorithm.UserOptimalAuction
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPRCostOps
import edu.colorado.fitzgero.sotestbed.algorithm.routing.FlowObservationOps
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithmV3
import edu.colorado.fitzgero.sotestbed.algorithm.batching.NetworkZoneBatching
import scala.util.Random

//import kantan.csv._
//import kantan.csv.ops._

case class MATSimExperimentRunner3(matsimRunConfig: MATSimRunConfig) extends LazyLogging {

  // val rng = matsimRunConfig.matsimRunSeed match {
  //   case None       => new Random()
  //   case Some(seed) => new Random(seed)
  // }

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
      agentsUnderControlPercentage = if (matsimRunConfig.algorithm
                                           .isInstanceOf[MATSimConfig.Algorithm.Selfish]) 0.0
      else matsimRunConfig.routing.adoptionRate
      agentsUnderControl <- matsimRunConfig.algorithm match {
        case _: Algorithm.Selfish => Right(Set.empty[Id[Person]])
        case uo: Algorithm.UserOptimalAuction =>
          val groupingFile: Option[String] = uo.selectionAlgorithm match {
            case ks: SelectionAlgorithmConfig.KarmaSelection if ks.driverPolicy.isInstanceOf[ExternalRLServer] =>
              ks.driverPolicy match {
                case ExternalRLServer(structure, client) =>
                  structure match {
                    case MultiAgentPolicy(space, groupingFile) => groupingFile
                    case SingleAgentPolicy(space)              => None
                  }
                case _ => throw new Exception
              }
            case rl: SelectionAlgorithmConfig.RLSelection => Some(rl.groupingFile)
            case _                                        => None
          }
          groupingFile match {
            case Some(gf) => PopulationOps.readGrouping(gf)
            case None =>
              PopulationOps.loadAgentsUnderControl(
                matsimRunConfig.populationFilepath.toFile,
                agentsUnderControlPercentage
              )
          }
        case so: Algorithm.SystemOptimal =>
          // do we have a grouping file? if so, load that
          val groupingFile: Option[String] = so.selectionAlgorithm match {
            case ks: SelectionAlgorithmConfig.KarmaSelection if ks.driverPolicy.isInstanceOf[ExternalRLServer] =>
              ks.driverPolicy match {
                case ExternalRLServer(structure, client) =>
                  structure match {
                    case MultiAgentPolicy(space, groupingFile) => groupingFile
                    case SingleAgentPolicy(space)              => None
                  }
                case _ => throw new Exception
              }
            case rl: SelectionAlgorithmConfig.RLSelection => Some(rl.groupingFile)
            case _                                        => None
          }
          groupingFile match {
            case Some(gf) => PopulationOps.readGrouping(gf)
            case None =>
              PopulationOps.loadAgentsUnderControl(
                matsimRunConfig.populationFilepath.toFile,
                agentsUnderControlPercentage
              )
          }

      }
      config = matsimRunConfig.copy(agentsUnderControl = agentsUnderControl)
    } yield {

      Files.createDirectories(config.experimentLoggingDirectory)

      // build some cost functions
      val freeFlowCostFunction: EdgeBPR => Cost = (edgeBPR: EdgeBPR) =>
        FreeFlowCostFunctionConfig.TravelTimeBased.getFreeFlow(edgeBPR)
      val costFunction: EdgeBPR => Cost = config.algorithm.costFunction

      // how we read reports
      val routingReporter: RoutingReports[IO, Coordinate, EdgeBPR] =
        config.io.routingReportConfig match {
          case RoutingReportConfig.Inactive =>
            RoutingReportConfig.Inactive.build()
          case RoutingReportConfig.ReplanningCoordinate =>
            RoutingReportConfig.ReplanningCoordinate.build(config.experimentLoggingDirectory)
          case RoutingReportConfig.CoreReporting =>
            RoutingReportConfig.CoreReporting.build(config.experimentLoggingDirectory, costFunction)
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

      val soAssetsOrError: Either[Error, Option[(RoutingAlgorithmV3, Map[String, Karma])]] =
        config.algorithm match {

          case so: Algorithm.SystemOptimal =>
            val soAlgorithmOrError = for {
              grid <- so.grid.build(network)
              gridFile = config.experimentLoggingDirectory.resolve("grid.csv").toFile
              _ <- CoordinateGrid2PrintOps.writeGridToCsv(grid, gridFile)
              _ <- checkRLKarmaUsesFreeFlow(so)
              // _    <- startEpisodesForRLPolicies(so.selectionAlgorithm, agentsUnderControl)
            } yield {
              val ksp: AltPathsAlgorithmRunner = {
                AltPathsAlgorithmRunner(
                  altPathsAlgorithm = so.kspAlgorithm.build(),
                  kspFilterFunction = so.kspFilterFunction.build(),
                  costFunction = costFunction,
                  freeFlowCostFunction = freeFlowCostFunction,
                  useFreeFlowNetworkCostsInPathSearch = so.useFreeFlowNetworkCostsInPathSearch,
                  seed = matsimRunConfig.matsimRunSeed.getOrElse(System.currentTimeMillis.toInt)
                )
              }
              val selectionAlgorithm: SelectionAlgorithm =
                so.selectionAlgorithm.build(config.experimentLoggingDirectory)

              selectionAlgorithm match {
                case ksa: KarmaSelectionAlgorithm =>
                  ksa.driverPolicy match {
                    case RLBasedDriverPolicy(structure, client) =>
                    // side effect here
                    case _ => ()
                  }
                case _ => ()
              }

              val sel: SelectionRunner =
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

              val alg = BatchedKspSelectionRouting(
                altPathsAlgorithmRunner = ksp,
                batchingFunction = so.batchingFunction.build(grid),
                batchFilterFunction =
                  so.batchFilterFunction.build(Some(config.routing.minBatchSearchSpace), grid, costFunction),
                selectionRunner = sel,
                k = so.kspAlgorithm.k,
//                minSearchSpaceSize = config.routing.minBatchSearchSpace
                minBatchSize = config.routing.minBatchSize,
                replanAtSameLink = config.routing.replanAtSameLink
              )
              Some(alg, bank)
            }

            soAlgorithmOrError

          case auction: Algorithm.UserOptimalAuction =>
            val gridResult = for {
              grid <- auction.grid.build(network)
              gridFile = config.experimentLoggingDirectory.resolve("grid.csv").toFile
              _ <- CoordinateGrid2PrintOps.writeGridToCsv(grid, gridFile)
              _ <- checkRLKarmaUsesFreeFlow(auction)
            } yield grid

            val bankResult: Either[Error, Map[String, Karma]] = auction.selectionAlgorithm match {
              case k: SelectionAlgorithmConfig.KarmaSelection =>
                val agentStrings = agentsUnderControl.map { _.toString }
                val bank         = k.bankConfig.build(agentStrings)
                Right(bank)
              case _ =>
                Left(new Error(s"using UserOptimalAuction routing but no bank config provided"))
            }

            val selectionAlgorithm: SelectionAlgorithm =
              auction.selectionAlgorithm.build(config.experimentLoggingDirectory)

            selectionAlgorithm match {
              case ksa: KarmaSelectionAlgorithm =>
                ksa.driverPolicy match {
                  case RLBasedDriverPolicy(structure, client) =>
                  // side effect here
                  case _ => ()
                }
              case _ => ()
            }

            val sel: SelectionRunner =
              SelectionRunner(
                selectionAlgorithm = selectionAlgorithm,
                pathToMarginalFlowsFunction = FlowObservationOps.defaultMarginalFlow,
                combineFlowsFunction = FlowObservationOps.defaultCombineFlows,
                marginalCostFunction = EdgeBPRCostOps.marginalCostFunction(0.15, 4.0),
                minimumAverageImprovement = config.routing.minimumAverageImprovement
              )

            val algResult = for {
              grid <- gridResult
              bank <- bankResult
              batchingFunction = auction.networkZoneBatching.build(grid).asInstanceOf[NetworkZoneBatching[_]]
            } yield UserOptimalAuctionSelection(
              networkZoneBatching = batchingFunction,
              selectionRunner = sel,
              replanAtSameLink = config.routing.replanAtSameLink,
              useCurrentLinkFlows = !auction.useFreeFlowNetworkCostsInPathSearch,
              theta = auction.newRouteDissimilarityPercent
            )

            val result = for {
              alg  <- algResult
              bank <- bankResult
            } yield Some((alg, bank))

            result

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
          costFunction = costFunction,
          bank = bank,
          updateFunction = config.algorithm.edgeUpdateFunction.build(),
          batchWindow = config.routing.batchWindow,
          minRequestUpdateThreshold = config.routing.minRequestUpdateThreshold,
          loggingDirectory = config.experimentLoggingDirectory
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

            val sel = ra match {
              case b: BatchedKspSelectionRouting  => Some(b.selectionRunner.selectionAlgorithm)
              case a: UserOptimalAuctionSelection => Some(a.selectionRunner.selectionAlgorithm)
              case _                              => None
            }

            sel match {
              case None =>
                IO.unit
              case Some(selectionAlgorithm) =>
                selectionAlgorithm match {
                  case rlsa: RLSelectionAlgorithm =>
                    for {
                      _ <- bankResult
                      // _ <- rlsa.reportAgentsAreDone()
                      _ <- rlsa.close()
                    } yield ()
                  case _ =>
                    bankResult
                }
            }

          case _ =>
            IO.unit
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

  def checkRLKarmaUsesFreeFlow(alg: Algorithm): Either[Error, Unit] = {
    alg match {
      case _: Selfish =>
        Left(new Error(s"internal error"))
      case so: SystemOptimal
          if so.selectionAlgorithm.isInstanceOf[RLSelection] && !so.useFreeFlowNetworkCostsInPathSearch =>
        Left(new Error(s"when using RL selection algorithm, useFreeFlowNetworkCostsInPathSearch must be true"))
      // case so: SystemOptimal => Right(so.selectionAlgorithm)
      case uoa: UserOptimalAuction
          if uoa.selectionAlgorithm.isInstanceOf[RLSelection] && !uoa.useFreeFlowNetworkCostsInPathSearch =>
        Right(uoa.selectionAlgorithm)
        Left(new Error(s"when using RL selection algorithm, useFreeFlowNetworkCostsInPathSearch must be true"))
      case _ => Right(())
    }
  }

  // def startEpisodesForRLPolicies(
  //   sac: SelectionAlgorithmConfig,
  //   agentsUnderControl: Set[Id[Person]]
  // ): Either[Error, Unit] = {
  //   val program = sac match {
  //     case rls: RLSelection =>
  //       IO.raiseError(new NotImplementedError)
  //     case ks: KarmaSelection =>
  //       ks.driverPolicy match {
  //         case erls: ExternalRLServer =>
  //           val requests = agentsUnderControl.toList.map { personId =>
  //             PolicyClientRequest.StartEpisodeRequest(
  //               episode_id = Some(EpisodeId(personId.toString)),
  //               training_enabled = erls.client.trainingEnabled
  //             )
  //           }
  //           erls.client.send(requests).map { _ => () }

  //         case _ => IO.unit
  //       }
  //     case _ => IO.unit
  //   }

  //   // temp hack to down-shift to an Either context
  //   import cats.effect.unsafe.implicits.global
  //   Try {
  //     program.unsafeRunSync
  //   }.toEither.left.map { t => new Error(s"failed starting RL episodes", t) }
  // }

}

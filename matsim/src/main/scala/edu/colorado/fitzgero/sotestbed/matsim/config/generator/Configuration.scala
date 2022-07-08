package edu.colorado.fitzgero.sotestbed.matsim.config.generator

import java.io.File

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.algorithm.selection.rl.Space
import edu.colorado.fitzgero.sotestbed.matsim.config.generator.ScalaUtilRandomOps._
import kantan.csv._

sealed trait Configuration

object Configuration {

  case class SelfishConfig(
    namePrefix: String,
    name: String,
    scenario: Scenario,
    batchWindow: Int,
    adoptionRate: Double,
    popSize: Int,
    bprAlpha: Double,
    bprBeta: Double
  ) extends Configuration

  case class SysOptConfig(
    namePrefix: String,
    name: String,
    scenario: Scenario,
    batchWindow: Int,
    adoptionRate: Double,
    popSize: Int,
    bprAlpha: Double,
    bprBeta: Double,
    altPathsFunction: AltPathsFunction,
    batchingFunction: BatchingFunction,
    batchFilterFunction: BatchFilterFunction,
    assignmentAlgorithm: AssignmentAlgorithm
  ) extends Configuration

  def generate(random: Random, prefixOption: Option[String] = None): List[Configuration] = {

    val namePrefix = prefixOption.getOrElse(s"${System.currentTimeMillis}")

    // where is this happening?
//    val scenario = Scenario.randomPick(random)
    val scenario = Scenario.Lafayette

    // high-level algorithm parameters
    val batchWindow = 30
    // val batchWindow   = random.uniformInRange(5, 30)
    val batchWindowMs = batchWindow * 1000
    val adoptionRate = 0.20
    // val adoptionRate  = random.uniformInRange(0.1, 1.0)

//    val popSize = {
//      val (low, high) = scenario match {
////        case Scenario.Golden    => (8000, 16000)
//        case Scenario.Lafayette => (7000, 18000)
////        case Scenario.Boulder   => (17000, 30000)
//      }
//      random.uniformInRange(low, high)
//    }
    val popSize  = 9000
    val bprAlpha = 0.15 // random.gaussianInRange(0.05, 0.15, 0.25, 0.08)
    val bprBeta  = 4.0 // random.gaussianInRange(2, 4, 6, 1)

    // alt paths parameters
    val altsK          = random.uniformInRange(5, 10)
    val altsTheta      = random.uniformInRange(0.0, 1.0)
    val altsIterations = random.uniformInRange(altsK, altsK * 2)

    // runtime split between batching and assignment
    // batching as 1/4 of the compute budget, up to 10 seconds
    val maxBatchingRuntime  = if (batchWindow > 20) 10000 else batchWindowMs / 4
    val batchingRuntimeMs   = random.uniformInRange(1000, maxBatchingRuntime)
    val assignmentRuntimeMs = batchWindowMs - batchingRuntimeMs

    // sub-batching parameters
    // val bfOmegaDelta              = random.uniformInRange(0.0, 1.0)
    // val bfOmegaBeta               = 1.0 - bfOmegaDelta
    // val bfOmegaA                  = random.uniformInRange(0.0, 1.0)
    // val bfOmegaS                  = 1.0 - bfOmegaA
    // val bfTrajHistoryLimitSeconds = random.uniformInRange(30, 200)
    val bfOmegaDelta              = 0.5
    val bfOmegaBeta               = 0.5
    val bfOmegaA                  = 0.5
    val bfOmegaS                  = 0.5
    val bfTrajHistoryLimitSeconds = 180

    // batch filter parameter
    // val bffSubBatchK = random.gaussianInIntegerRange(10, 40, 100, Some(20))
    val bffSubBatchK = 50

    // assignment parameter
    val assignmentExloredPct = 0.1 //random.gaussianInRange(0.00000001, 0.1, 1.0, 0.005)

    val altsConfig = AltPathsFunction.KSPWithLimitedOverlap(altsK, altsTheta, altsIterations)
    val batchingFunctionConfig = BatchingFunction.TrajectoryClustering(
      bfOmegaDelta,
      bfOmegaBeta,
      bfOmegaA,
      bfOmegaS,
      batchingRuntimeMs,
      bfTrajHistoryLimitSeconds
    )
//    val batchingFunctionConfig = BatchingFunction.NoBatching
    val batchFilterFunctionConfig = BatchFilterFunction.TopK(bffSubBatchK)
//    val batchFilterFunctionConfig = BatchFilterFunction.NoFilter
    val selfishAlgorithm = SelfishConfig(
      namePrefix,
      s"""$namePrefix-selfish""",
      scenario,
      batchWindow,
      adoptionRate,
      popSize,
      bprAlpha,
      bprBeta
    )

    // val mctsCoefficientInput = random.gaussianInRange(2.0, 4.0, 16.0, 4)
    // val mctsCoefficient      = 2.0 / math.sqrt(mctsCoefficientInput)
    val mctsCoefficient      = 2.0 / math.sqrt(2.0)
    val soAlgorithms = List(
      AssignmentAlgorithm.Base,
      AssignmentAlgorithm.Karma(1, 1, 100),
      AssignmentAlgorithm.Rand(assignmentRuntimeMs, bffSubBatchK, assignmentExloredPct),
      AssignmentAlgorithm.Mcts(mctsCoefficient, assignmentRuntimeMs, bffSubBatchK, assignmentExloredPct)
//      AssignmentAlgorithm.Rl("http://localhost", 9900, Space.V1, new File("grouping.json"))
    )

    val confs: List[SysOptConfig] = for {
      assignmentConfig <- soAlgorithms
    } yield SysOptConfig(
      namePrefix,
      s"""$namePrefix-${assignmentConfig.algorithmName}""",
      scenario,
      batchWindow,
      adoptionRate,
      popSize,
      bprAlpha,
      bprBeta,
      altsConfig,
      batchingFunctionConfig,
      batchFilterFunctionConfig,
      assignmentConfig
    )

    selfishAlgorithm +: confs
  }

  implicit class ConfigurationOps(configuration: Configuration) {

    def name: String = configuration match {
      case c: SelfishConfig => c.name
      case c: SysOptConfig  => c.name
    }

    def sortOrder: Double = configuration match {
      case c: Configuration.SelfishConfig => c.popSize * c.adoptionRate
      case c: Configuration.SysOptConfig  => c.popSize * c.adoptionRate
    }

    def grouping: String = configuration match {
      case c: Configuration.SelfishConfig => c.namePrefix
      case c: Configuration.SysOptConfig  => c.namePrefix
    }

    def populationSize: Int = configuration match {
      case c: Configuration.SelfishConfig => c.popSize
      case c: Configuration.SysOptConfig  => c.popSize
    }

    def filename: String = s"${configuration.name}.conf"

    def toHocon: String = {

      configuration match {
        case SelfishConfig(namePrefix, name, scenario, batchWindow, adoptionRate, popSize, bprAlpha, bprBeta) =>
          val defaultConfig    = Default(bprAlpha, bprBeta)
          val outNameConfig    = s"""name = "$name"\n"""
          val populationConfig = Population(popSize)
          val routingConfig    = Routing(adoptionRate, batchWindow)

          val selfishAlgorithm =
            s"""algorithm {
               |  type = selfish
               |  name = "selfish"
               |  edge-update-function.type = flow-count
               |  marginal-cost-function.type = capacity-cost-function
               |}""".stripMargin

          val hocon = List(
            outNameConfig,
            scenario.toHocon,
            selfishAlgorithm,
            populationConfig,
            routingConfig,
            defaultConfig
          ).mkString("\n")

          hocon

        case SysOptConfig(
            namePrefix,
            name,
            scenario,
            batchWindow,
            adoptionRate,
            popSize,
            bprAlpha,
            bprBeta,
            altPathsFunction,
            batchingFunction,
            batchFilterFunction,
            assignmentAlgorithm
            ) =>
          val defaultConfig    = Default(bprAlpha, bprBeta)
          val outNameConfig    = s"""name = "$name"\n"""
          val populationConfig = Population(popSize)
          val routingConfig    = Routing(adoptionRate, batchWindow, bprAlpha = bprAlpha, bprBeta = bprBeta)

          val hocon = List(
            outNameConfig,
            scenario.toHocon,
            assignmentAlgorithm.toHocon,
            altPathsFunction.toHocon,
            batchingFunction.toHocon,
            batchFilterFunction.toHocon,
            populationConfig,
            routingConfig,
            defaultConfig
          ).mkString("\n")

          hocon
      }
    }
  }

  implicit val ConfigurationHeaderEncoder: HeaderEncoder[Configuration] = {
    HeaderEncoder.encoder(
      "name",
      "scenario",
      "adoptionRate",
      "batchWindow",
      "popSize",
      "algorithm",
      "bprAlpha",
      "bprBeta",
      "altsK",
      "altsTheta",
      "altsIterations",
      "batchingRuntimeMs",
      "assignmentRuntimeMs",
      "bfOmegaDelta",
      "bfOmegaBeta",
      "bfOmegaA",
      "bfOmegaS",
      "bfTrajHistoryLimitSeconds",
      "bffSubBatchK",
      "mctsCoefficient",
      "assignmentExloredPct"
    ) { configuration: Configuration =>
      configuration match {
        case c: SelfishConfig =>
          (
            c.name,
            c.scenario.toString,
            c.adoptionRate.toString,
            c.batchWindow.toString,
            c.popSize.toString,
            "selfish",
            c.bprAlpha.toString,
            c.bprBeta.toString,
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            ""
          )
        case c: SysOptConfig =>
          c.altPathsFunction match {
            case AltPathsFunction.KSPWithLimitedOverlap(k, theta, searchIterations) =>
              val (omegaDelta, omegaBeta, omegaA, omegaS, runtimeMs, trajectoryHistoryTimeLimitSeconds) =
                c.batchingFunction match {
                  case BatchingFunction.NoBatching =>
                    (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
                  case BatchingFunction.TrajectoryClustering(
                      omegaDelta,
                      omegaBeta,
                      omegaA,
                      omegaS,
                      runtimeMs,
                      trajectoryHistoryTimeLimitSeconds
                      ) =>
                    (omegaDelta, omegaBeta, omegaA, omegaS, runtimeMs, trajectoryHistoryTimeLimitSeconds)
                }

              val subBatchK = c.batchFilterFunction match {
                case BatchFilterFunction.NoFilter => ""
                case BatchFilterFunction.TopK(k)  => k.toString
              }

              val (compBudget, exploredBudget) = c.assignmentAlgorithm match {
                case AssignmentAlgorithm.Base => ("", "")
                case rand: AssignmentAlgorithm.Rand =>
                  (rand.computeBudgetMs.toString, rand.exploredBudget.toString)
                case mcts: AssignmentAlgorithm.Mcts =>
                  (mcts.computeBudgetMs.toString, mcts.exploredBudget.toString)
                case _: AssignmentAlgorithm.Karma => ("", "")
                case _: AssignmentAlgorithm.Rl    => ("", "")
              }

              val mctsCoefficient = c.assignmentAlgorithm match {
                case a: AssignmentAlgorithm.Mcts => a.mctsCoefficient.toString
                case _                           => ""
              }

              val row = (
                c.name,
                c.scenario.toString,
                c.adoptionRate.toString,
                c.batchWindow.toString,
                c.popSize.toString,
                c.assignmentAlgorithm.algorithmName,
                c.bprAlpha.toString,
                c.bprBeta.toString,
                k.toString,
                theta.toString,
                searchIterations.toString,
                runtimeMs.toString,
                compBudget,
                omegaDelta.toString,
                omegaBeta.toString,
                omegaA.toString,
                omegaS.toString,
                trajectoryHistoryTimeLimitSeconds.toString,
                subBatchK,
                mctsCoefficient,
                exploredBudget
              )

              row
          }
      }
    }
  }
}

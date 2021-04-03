package edu.colorado.fitzgero.sotestbed.matsim.config.generator

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.matsim.config.generator.ScalaUtilRandomOps._
import kantan.csv._
import kantan.csv.ops._

sealed trait Configuration

object Configuration {

  case class SelfishConfig(
    name: String,
    scenario: Scenario,
    batchWindow: Int,
    adoptionRate: Double,
    popSize: Int,
    bprAlpha: Double,
    bprBeta: Double
  ) extends Configuration

  case class SysOptConfig(
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
    val scenario = Scenario.randomPick(random)

    // high-level algorithm parameters
    val batchWindow   = random.uniformInRange(5, 600)
    val batchWindowMs = batchWindow * 1000
    val adoptionRate  = random.uniformInRange(0.0, 1.0)
    val popSize = {
      val (low, high) = scenario.popSizeRange
      random.uniformInRange(low, high)
    }
    val maxBatchingRuntime = if (batchWindow > 20) 10000 else batchWindowMs / 2
    val bprAlpha           = 0.15
    val bprBeta            = 4.0

    // alt paths parameters
    val altsK          = random.uniformInRange(2, 20)
    val altsTheta      = random.gaussianInRange(0.0, 0.5, 1.0)
    val altsIterations = random.uniformInRange(altsK, altsK * 2)

    // runtime split between batching and assignment
    val batchingRuntimeMs   = random.uniformInRange(1000, maxBatchingRuntime)
    val assignmentRuntimeMs = batchWindowMs - batchingRuntimeMs

    // sub-batching parameters
    val bfOmegaDelta              = random.gaussianInRange(0.0, 0.5, 1.0)
    val bfOmegaBeta               = 1.0 - bfOmegaDelta
    val bfOmegaA                  = random.gaussianInRange(0.0, 0.5, 1.0)
    val bfOmegaS                  = 1.0 - bfOmegaA
    val bfTrajHistoryLimitSeconds = random.gaussianInRange(60, 150, 300)

    // batch filter parameter
    val bffSubBatchK = random.uniformInRange(1, 20)

    // assignment parameter
    val assignmentExloredPct = random.gaussianInRange(0.00000001, 0.1, 1.0)

    val altsConfig = AltPathsFunction.KSPWithLimitedOverlap(altsK, altsTheta, altsIterations)
    val batchingFunctionConfig = BatchingFunction.TrajectoryClustering(
      bfOmegaDelta,
      bfOmegaBeta,
      bfOmegaA,
      bfOmegaS,
      batchingRuntimeMs,
      bfTrajHistoryLimitSeconds
    )
    val batchFilterFunctionConfig = BatchFilterFunction.TopK(bffSubBatchK)
    val selfishAlgorithm = SelfishConfig(
      s"""$namePrefix-selfish""",
      scenario,
      batchWindow,
      adoptionRate,
      popSize,
      bprAlpha,
      bprBeta
    )
    val soAlgorithms = List(
      AssignmentAlgorithm.Base,
      AssignmentAlgorithm.Rand(assignmentRuntimeMs, bffSubBatchK, assignmentExloredPct),
      AssignmentAlgorithm.Mcts(assignmentRuntimeMs, bffSubBatchK, assignmentExloredPct)
    )

    val confs: List[SysOptConfig] = for {
      assignmentConfig <- soAlgorithms
    } yield SysOptConfig(
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

    def filename: String = s"${configuration.name}.conf"

    def toHocon: String = {

      val defaultConfig = Default()

      configuration match {
        case SelfishConfig(name, scenario, batchWindow, adoptionRate, popSize, bprAlpha, bprBeta) =>
          val outNameConfig    = s"""name = "$name"\n"""
          val populationConfig = Population(popSize)
          val routingConfig    = Routing(adoptionRate, batchWindow)

          val selfishAlgorithm =
            s"""algorithm {
               |  type = selfish
               |  name = "selfish"
               |  edge-update-function.type = flow-count
               |  marginal-cost-function = {
               |    type = edge-bpr-function
               |    alpha = $bprAlpha
               |    beta = $bprBeta
               |  }
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
            ""
          )
        case c: SysOptConfig =>
          c.altPathsFunction match {
            case AltPathsFunction.KSPWithLimitedOverlap(k, theta, searchIterations) =>
              c.batchingFunction match {
                case BatchingFunction.TrajectoryClustering(
                    omegaDelta,
                    omegaBeta,
                    omegaA,
                    omegaS,
                    runtimeMs,
                    trajectoryHistoryTimeLimitSeconds
                    ) =>
                  c.batchFilterFunction match {
                    case BatchFilterFunction.TopK(subBatchK) =>
                      val (compBudget, exploredBudget) = c.assignmentAlgorithm match {
                        case AssignmentAlgorithm.Base => ("", "")
                        case rand: AssignmentAlgorithm.Rand =>
                          (rand.computeBudgetMs.toString, rand.exploredBudget.toString)
                        case mcts: AssignmentAlgorithm.Mcts =>
                          (mcts.computeBudgetMs.toString, mcts.exploredBudget.toString)
                      }
                      //
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
                        subBatchK.toString,
                        exploredBudget
                      )

                      row
                  }
              }
          }
      }
    }
  }
}

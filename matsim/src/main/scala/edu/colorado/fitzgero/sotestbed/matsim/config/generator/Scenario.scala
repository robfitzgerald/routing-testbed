package edu.colorado.fitzgero.sotestbed.matsim.config.generator

import java.io.File
import java.nio.file.{Path, Paths}

import scala.util.Random

sealed trait Scenario extends Product with Serializable

object Scenario {

  case object Golden  extends Scenario
  case object Boulder extends Scenario

  val All = List(Golden, Boulder)

  def randomPick(random: Random): Scenario = {
    random.nextInt(2) match {
      case 0 => Golden
      case 1 => Boulder
      case n => throw new IllegalStateException(s"random next int $n should not be possible")
    }
  }

  implicit class ScenarioOps(scenario: Scenario) {

    def networkFilePath: Path = {
      val resourceDirectory: Path = Paths.get("matsim/src/main/resources/matsim-resources-2021")
      scenario match {
        case Golden  => resourceDirectory.resolve("Golden_CO.xml")
        case Boulder => resourceDirectory.resolve("Boulder_CO.xml")
      }
    }

    def popSizeRange: (Int, Int) = scenario match {
      case Golden  => (5000, 20000)
      case Boulder => (10000, 30000)
    }

    def toHocon: String = scenario match {
      case Golden =>
        s"""io {
           |  matsim-network-file = "Golden_CO.xml"
           |  matsim-config-file = "matsim-config.xml"
           |  name-prefix = "matsim-golden-co"
           |  matsim-log-level = "WARN"
           |}
           |
           |algorithm {
           |  grid {
           |    min-x = -11715308.93
           |    max-x = -11706548.83
           |    min-y = 4822215.1
           |    max-y = 4834563.74
           |    grid-cell-side-length = 1000
           |    srid = 3857
           |  }
           |}""".stripMargin
      case Boulder =>
        s"""io {
           |  matsim-network-file = "Boulder_CO.xml"
           |  matsim-config-file = "matsim-config.xml"
           |  name-prefix = "matsim-boulder-co"
           |  matsim-log-level = "WARN"
           |}
           |
           |algorithm {
           |  grid {
           |    min-x = -11721773.66
           |    max-x = -11712796.04
           |    min-y = 4860786.76
           |    max-y = 4875939.84
           |    grid-cell-side-length = 1000
           |    srid = 3857
           |  }
           |}""".stripMargin
    }
  }
}

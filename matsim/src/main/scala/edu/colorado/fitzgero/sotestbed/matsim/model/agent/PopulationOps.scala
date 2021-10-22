package edu.colorado.fitzgero.sotestbed.matsim.model.agent

import java.io.File

import scala.util.{Random, Try}
import scala.xml.XML

import edu.colorado.fitzgero.sotestbed.util.XMLParserIgnoresDTD
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.population.Person
import cats.implicits._

import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.parser._

object PopulationOps {

  /**
    * reads a list of agents under control for RL-based algorithms
    * @param groupingFile file with agent groupings
    * @return either an error, or the agent ids listed in the grouping file
    *         based on [[https://docs.ray.io/en/latest/rllib-env.html#grouping-agents]]
    */
  def readGrouping(groupingFile: File): Either[Throwable, Set[Id[Person]]] = {
    val fileStringOrError = Try {
      val source = scala.io.Source.fromFile(groupingFile)
      val string = source.getLines.mkString
      source.close
      string
    }.toEither

    val result = for {
      fileString <- fileStringOrError
      grouping   <- decode[Map[String, List[String]]](fileString)
      agents = grouping.values.flatten.toList
      persons <- agents.traverse { str => Try { Id.create(str, classOf[Person]) } }.toEither
    } yield {
      persons.toSet
    }

    result
  }

  /**
    * given an adoption rate, sample the percentage of agents to assign SO routing objectives
    * @param populationFile the source population file
    * @param adoptionRate
    * @param seed
    * @return
    */
  def loadAgentsUnderControl(
    populationFile: File,
    adoptionRate: Double,
    seed: Option[Long] = None
  ): Either[Throwable, Set[Id[Person]]] = {

    val validRate: Boolean = 0.0 <= adoptionRate && adoptionRate <= 1.0
    assert(validRate, f"adoption rate $adoptionRate must be in range [0, 1]")

    Try {

      val population: xml.Elem = XML.withSAXParser(XMLParserIgnoresDTD.parser).loadFile(populationFile)

      val random: Random = new Random(seed.getOrElse(System.currentTimeMillis))

      val populationSet: Seq[Id[Person]] =
        (population \ "person")
          .map { node => Id.create(node.attributes.asAttrMap("id"), classOf[Person]) }

      val populationSize: Int = populationSet.size
      val targetSize: Int     = ((populationSize.toDouble) * adoptionRate).toInt

      val result: Set[Id[Person]] = random.shuffle(populationSet).take(targetSize).toSet

      result

    }.toEither
  }
}

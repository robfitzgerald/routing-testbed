package edu.colorado.fitzgero.sotestbed.matsim.model.agent

import java.io.File

import scala.util.{Random, Try}
import scala.xml.XML

import edu.colorado.fitzgero.sotestbed.util.XMLParserIgnoresDTD
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.population.Person

object PopulationOps {

  /**
    * given an adoption rate, sample the percentage of agents to assign SO routing objectives
    * @param populationFile the source population file
    * @param adoptionRate
    * @param seed
    * @return
    */
  def loadAgentsUnderControl(populationFile: File, adoptionRate: Double, seed: Option[Long] = None): Either[Throwable, Set[Id[Person]]] = {

    val validRate: Boolean = 0.0 <= adoptionRate && adoptionRate <= 1.0
    assert(validRate, f"adoption rate $adoptionRate must be in range [0, 1]")

    Try {

      val population: xml.Elem = XML.withSAXParser(XMLParserIgnoresDTD.parser).loadFile(populationFile)

      val random: Random = new Random(seed.getOrElse(System.currentTimeMillis))

      val populationSet: Seq[Id[Person]] =
        (population \ "person")
          .map { node =>
            Id.create(node.attributes.asAttrMap("id"), classOf[Person])
          }

      val populationSize: Int = populationSet.size
      val targetSize: Int     = ((populationSize.toDouble) * adoptionRate).toInt

      val result: Set[Id[Person]] = random.shuffle(populationSet).take(targetSize).toSet

      result

    }.toEither
  }
}

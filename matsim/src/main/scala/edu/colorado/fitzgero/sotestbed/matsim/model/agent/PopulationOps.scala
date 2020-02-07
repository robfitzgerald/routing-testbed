package edu.colorado.fitzgero.sotestbed.matsim.model.agent

import java.io.File

import scala.util.Try
import scala.xml.XML

import edu.colorado.fitzgero.sotestbed.util.XMLParserIgnoresDTD
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.population.Person

object PopulationOps {
  def loadAgentsUnderControl(populationFile: File): Either[Throwable, Set[Id[Person]]] = {
    Try {

      val population: xml.Elem = XML.withSAXParser(XMLParserIgnoresDTD.parser).loadFile(populationFile)

      (population \ "person")
        .filter{_.child.toString.contains("so")} // TODO: should explicitly inspect person.attribute@requestClass
        .map{node => Id.create(node.attributes.asAttrMap("id"), classOf[Person])}
        .toSet[Id[Person]]

    }.toEither
  }
}

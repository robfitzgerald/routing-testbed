package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.File

import scala.xml.XML
import scala.xml.dtd._

import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.MATSimPopConfig
import com.typesafe.config.ConfigFactory
import pureconfig._
import pureconfig.generic.auto._
import MATSimPopConfig.localDateConvert

object MATSimPopulationApp extends App {
  val result = for {
    config <- ConfigSource.fromConfig(ConfigFactory.parseFile(new File("matsim/src/main/resources/matsim-conf/rye/default-population.conf"))).load[MATSimPopConfig]
    popSamplingAlgorithm <- config.pop.popSampling.build(config)
    population = popSamplingAlgorithm.generate
  } yield {

    // converts each agent to xml, announcing any errors along the way
    val (agents, failures) = population.foldLeft((List.empty[xml.Elem], 0)){ (acc, agent) =>

      agent.toXML match {
        case Right(a) =>
          (a +: acc._1, acc._2)
        case Left(e) =>
          println(e)
          (acc._1, acc._2 + 1)
      }
    }

    if (failures > 0) {
      println(s"$failures agent generation failures resulting in population of size ${agents.length}")
    }

    // write new population to disk
    XML.save(
      config.fs.populationFileDestination.toString,
      <population>
        {agents}
      </population>,
      "UTF8",
      xmlDecl = true,
      DocType("population", SystemID("matsim/src/main/resources/matsim-dtd/population_v6.dtd"), Nil)
    )
  }

  println(result)
}

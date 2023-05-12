package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population

import edu.colorado.fitzgero.sotestbed.matsim.model.agent.Agent
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.PopSampling
import java.nio.file.Path
import scala.xml.XML
import scala.xml.dtd._
import cats.implicits._
import scala.util.Try
import com.typesafe.scalalogging.LazyLogging

trait PopSamplingAlgorithm {
  def generate: Either[Error, List[Agent]]
}

object PopSamplingAlgorithm extends LazyLogging {

  /**
    * creates a population sampling algorithm, builds a population from that input, and writes the population
    * in MATSim population_v6.dtd format to the provided directory.
    */
  def generatePopulation(popSampling: PopSampling, filePath: Path, trial: Option[Int]): Either[Error, Unit] = {
    for {
      alg        <- popSampling.build
      population <- alg.generate
      agents     <- population.traverse(_.toXML).left.map { f => new Error(f"failure building agents: ${f}") }
      _ = logger.info(f"generated ${agents.length} agents for population file")
      _ <- saveXml(filePath, agents)
      _ = logger.info(f"population written to $filePath")
    } yield ()
  }

  def saveXml(filePath: Path, agents: List[xml.Elem]): Either[Error, Unit] = {
    Try {
      XML.save(
        filePath.toString,
        <population>
            {agents}
          </population>,
        "UTF8",
        xmlDecl = true,
        DocType("population", SystemID("matsim/src/main/resources/matsim-dtd/population_v6.dtd"), Nil)
      )
    }.toEither.left.map { t => new Error(s"failure saving XML population", t) }
  }

}

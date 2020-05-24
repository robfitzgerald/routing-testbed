package edu.colorado.fitzgero.sotestbed.matsim.model.roadnetwork

import scala.collection.JavaConverters._
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig
import edu.colorado.fitzgero.sotestbed.model.numeric.{Capacity, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork.EdgeTriplet
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.network.{Link, Network}
import org.matsim.core.config.{Config, ConfigUtils}
import org.matsim.core.controler.Controler
import org.matsim.core.network.NetworkUtils

object MATSimCapacitiesOverRoadNetwork extends LazyLogging {

  def apply(
    roadNetwork: LocalAdjacencyListFlowNetwork,
    config: MATSimConfig
  ): Either[Throwable, LocalAdjacencyListFlowNetwork] = {

    Try {
      // load a copy of the MATSim Scenario in order to inspect capacity values
      val matsimConfig: Config = ConfigUtils.loadConfig(config.io.matsimConfigFile.toString)
//      matsimConfig.controler.setOutputDirectory(config.io.experimentDirectory.resolve("tmp").toString)
//      matsimConfig.plans.setInputFile(config.io.populationFile.toString)
      matsimConfig.network.setInputFile(config.io.matsimNetworkFile.toString)
//      matsimConfig.controler.setLastIteration(config.routing.selfish.lastIteration)
//      val controler: Controler = new Controler(matsimConfig)
      val matsimNetwork: Network = NetworkUtils.createNetwork(matsimConfig)
      NetworkUtils.readNetwork(matsimNetwork, config.io.matsimNetworkFile.toString)

      val edgeLookup: Map[Id[Link], Link] = matsimNetwork.getLinks.asScala.toMap

      // update edge attributes with the MATSim network capacity value
      val (result, errors) = roadNetwork.edges.values.foldLeft((roadNetwork, List.empty[String])) { (acc, edgeTriplet) =>
        val (rn, errors) = acc
        edgeTriplet.attr match {
          case edgeBPR: EdgeBPR =>
            edgeLookup.get(Id.create(edgeTriplet.edgeId.value, classOf[Link])) match {
              case None =>
                (rn, f"failed to find edge ${edgeTriplet.edgeId} in MATSim network" +: errors)
              case Some(link) =>
                // not properly scaling this Capacity value here
                val cap: Capacity                            = Capacity(link.getCapacity.toInt)
                val updatedEdgeBPR: EdgeBPR                  = edgeBPR.copy(capacity = cap)
                val updatedEdgeTriplet: EdgeTriplet[EdgeBPR] = edgeTriplet.copy(attr = updatedEdgeBPR)
                val updatedRoadNetwork: LocalAdjacencyListFlowNetwork =
                  rn.copy(
                    edges = rn.edges.updated(edgeTriplet.edgeId, updatedEdgeTriplet)
                  )
                (updatedRoadNetwork, errors)

            }
        }
      }

      errors.foreach { e =>
        logger.error(e)
      }

      result
    }.toEither
  }
}

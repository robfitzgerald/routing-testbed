package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population
import java.io.File
import java.time.LocalTime

import scala.util.Random
import scala.collection.JavaConverters._

import edu.colorado.fitzgero.sotestbed.matsim.model.agent.AgentActivity.{Activity, FinalActivity, FirstActivity}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.{ActivityType, Agent, AgentActivityPair}
import edu.colorado.fitzgero.sotestbed.model.agent.{RequestClass, TravelMode}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.network.{Link, Network}
import org.matsim.core.network.NetworkUtils

case class UniformPopSamplingAlgorithm(
  roadNetwork: LocalAdjacencyListFlowNetwork[Coordinate, EdgeBPR],
  matsimNetwork: Network,
  populationSize: Int,
  percentSOAgents: Double,
  workActivityMinTime: LocalTime,
  workActivityMaxTime: LocalTime,
  workDurationHours: Int,
  seedOption: Option[Long],
) extends PopSamplingAlgorithm {

  val random: Random = seedOption match {
    case Some(seed) => new Random(seed)
    case None => Random
  }

  def generate: List[Agent] = {

    val links: Map[Id[Link], Link] = matsimNetwork.getLinks.asScala.toMap
    val edgesArray: Array[EdgeId]   = roadNetwork.edges.keys.toArray
    def randomEdge: EdgeId          = edgesArray(random.nextInt(edgesArray.length))

    val secondsBetweenMinAndMaxWorkTime: Int = workActivityMaxTime.minusSeconds(workActivityMinTime.toSecondOfDay).toSecondOfDay
    def sampleWorkTime: LocalTime   = workActivityMinTime.plusSeconds(random.nextInt(secondsBetweenMinAndMaxWorkTime))
    def isSoAgent: Boolean          = random.nextDouble < percentSOAgents

    val agents: Seq[Agent] = for {
      uniqueId <- 1 to populationSize
      homeLocation = randomEdge
      homeNode <- links.get(Id.createLinkId(homeLocation.value))
      homeCoord = homeNode.getCoord
      workLocation = randomEdge
      workNode <- links.get(Id.createLinkId(workLocation.value))
      workCoord = workNode.getCoord
      agentId      = s"$uniqueId-$homeLocation-$workLocation"
      requestClass = if (isSoAgent) RequestClass.SO() else RequestClass.UE
      workTime     = sampleWorkTime
      homeEndTime  = if (workTime.isAfter(LocalTime.parse("01:00:00"))) workTime.minusHours(1) else LocalTime.MIN
    } yield {

      val agent = Agent(
        agentId,
        requestClass,
        List(
          AgentActivityPair(
            FirstActivity(
              ActivityType.Home,
              homeLocation,
              homeCoord,
              homeEndTime
            ),
            Activity(
              ActivityType.Work,
              workLocation,
              workCoord,
              workTime,
              workTime.plusHours(workDurationHours)
            ),
            TravelMode.Car
          ),
          AgentActivityPair(
            Activity(
              ActivityType.Work,
              workLocation,
              workCoord,
              workTime,
              workTime.plusHours(workDurationHours)
            ),
            FinalActivity(
              ActivityType.Home,
              homeLocation,
              homeCoord
            ),
            TravelMode.Car
          )
        )
      )

      agent
    }

    agents.toList
  }
}

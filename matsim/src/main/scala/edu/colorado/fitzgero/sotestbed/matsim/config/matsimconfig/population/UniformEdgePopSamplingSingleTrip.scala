package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population

import java.time.LocalTime

import scala.collection.JavaConverters._
import scala.util.Random

import edu.colorado.fitzgero.sotestbed.matsim.model.agent.AgentActivity.{Activity, FinalActivity, FirstActivity}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.{ActivityType, Agent, AgentActivityPair}
import edu.colorado.fitzgero.sotestbed.model.agent.TravelMode
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.network.{Link, Network}

case class UniformEdgePopSamplingSingleTrip(
  roadNetwork: LocalAdjacencyListFlowNetwork,
  matsimNetwork: Network,
  populationSize: Int,
  workActivityMinTime: LocalTime,
  workActivityMaxTime: LocalTime,
  workDurationHours: Int,
  seedOption: Option[Long]
) extends PopSamplingAlgorithm {

  val random: Random = seedOption match {
    case Some(seed) => new Random(seed)
    case None       => Random
  }

  def generate: List[Agent] = {

    val links: Map[Id[Link], Link] = matsimNetwork.getLinks.asScala.toMap
    val edgesArray: Array[EdgeId]  = roadNetwork.edgesMap.keys.toArray
    def randomEdge: EdgeId         = edgesArray(random.nextInt(edgesArray.length))

    val secondsBetweenMinAndMaxWorkTime: Int =
      workActivityMaxTime.minusSeconds(workActivityMinTime.toSecondOfDay).toSecondOfDay
    def sampleWorkTime: LocalTime = workActivityMinTime.plusSeconds(random.nextInt(secondsBetweenMinAndMaxWorkTime))

    val agents: Seq[Seq[Agent]] = for {
      uniqueId <- 1 to populationSize
      homeLocation = randomEdge
      homeNode <- links.get(Id.createLinkId(homeLocation.value))
      homeCoord    = homeNode.getCoord
      workLocation = randomEdge
      workNode <- links.get(Id.createLinkId(workLocation.value))
      workCoord     = workNode.getCoord
      baseAgentId   = s"$uniqueId-$homeLocation-$workLocation"
      workTime      = sampleWorkTime
      homeEndTime   = if (workTime.isAfter(LocalTime.parse("01:00:00"))) workTime.minusHours(1) else LocalTime.MIN
      leaveWorkTime = workTime.plusHours(9)
    } yield {

      val agentId2 = baseAgentId + "-2"

      val agent1 = Agent(
        baseAgentId + "-1",
        List(
          AgentActivityPair(
            FirstActivity(
              ActivityType.Home,
              homeLocation,
              homeCoord,
              homeEndTime
            ),
            FinalActivity(
              ActivityType.Work,
              workLocation,
              workCoord
            ),
            TravelMode.Car
          )
        )
      )

      val agent2 = Agent(
        baseAgentId + "-2",
        List(
          AgentActivityPair(
            FirstActivity(
              ActivityType.Work,
              workLocation,
              workCoord,
              leaveWorkTime
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

      Seq(agent1, agent2)
    }

    agents.flatten.toList
  }
}

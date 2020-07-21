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

case class UniformEdgePopulationSamplingAlgorithm(
  roadNetwork: LocalAdjacencyListFlowNetwork,
  matsimNetwork: Network,
  populationSize: Int,
  workActivityMinTime: LocalTime,
  workActivityMaxTime: LocalTime,
  workDurationHours: Int,
  seedOption: Option[Long],
) extends PopSamplingAlgorithm {

  val random: Random = seedOption match {
    case Some(seed) => new Random(seed)
    case None       => Random
  }

  def generate: List[Agent] = {

    val links: Map[Id[Link], Link] = matsimNetwork.getLinks.asScala.toMap
    val edgesArray: Array[EdgeId]  = roadNetwork.edgesMap.keys.toArray
    def randomEdge: EdgeId         = edgesArray(random.nextInt(edgesArray.length))

    val secondsBetweenMinAndMaxWorkTime: Int = workActivityMaxTime.minusSeconds(workActivityMinTime.toSecondOfDay).toSecondOfDay
    def sampleWorkTime: LocalTime            = workActivityMinTime.plusSeconds(random.nextInt(secondsBetweenMinAndMaxWorkTime))

    val agents: Seq[Agent] = for {
      uniqueId <- 1 to populationSize
      homeLocation = randomEdge
      homeNode <- links.get(Id.createLinkId(homeLocation.value))
      homeCoord    = homeNode.getCoord
      workLocation = randomEdge
      workNode <- links.get(Id.createLinkId(workLocation.value))
      workCoord   = workNode.getCoord
      agentId     = s"$uniqueId-$homeLocation-$workLocation"
      workTime    = sampleWorkTime
      homeEndTime = if (workTime.isAfter(LocalTime.parse("01:00:00"))) workTime.minusHours(1) else LocalTime.MIN
    } yield {

      val homeMorning: FirstActivity = FirstActivity(
        ActivityType.Home,
        homeLocation,
        homeCoord,
        homeEndTime
      )
      val work: Activity = Activity(
        ActivityType.Work,
        workLocation,
        workCoord,
        workTime,
        LocalTime.of(workDurationHours, 0, 0)
//        workTime.plusHours(workDurationHours)
      )
      val homeEvening: FinalActivity = FinalActivity(
        ActivityType.Home,
        homeLocation,
        homeCoord
      )

      val agent = Agent(
        agentId,
        List(
          AgentActivityPair(homeMorning, work, TravelMode.Car),
          AgentActivityPair(work, homeEvening, TravelMode.Car)
        )
      )

      agent
    }

    agents.toList
  }
}

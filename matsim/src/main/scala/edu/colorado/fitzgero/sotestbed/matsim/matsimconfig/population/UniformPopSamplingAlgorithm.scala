package edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.population
import java.time.LocalTime

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.matsim.model.agent.AgentActivity.{Activity, FinalActivity, FirstActivity}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.{ActivityType, Agent, AgentActivityPair}
import edu.colorado.fitzgero.sotestbed.model.agent.{RequestClass, TravelMode}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

case class UniformPopSamplingAlgorithm(
  roadNetwork: LocalAdjacencyListFlowNetwork[Coordinate, EdgeBPR],
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
    val edgesArray: Array[EdgeId]   = roadNetwork.edges.keys.toArray
    def randomEdge: EdgeId          = edgesArray(random.nextInt(edgesArray.length))

    val secondsBetweenMinAndMaxWorkTime: Int = workActivityMaxTime.minusSeconds(workActivityMinTime.toSecondOfDay).toSecondOfDay
    def sampleWorkTime: LocalTime   = workActivityMinTime.plusSeconds(random.nextInt(secondsBetweenMinAndMaxWorkTime))
    def isSoAgent: Boolean          = random.nextDouble < percentSOAgents

    val agents: Seq[Agent] = for {
      uniqueId <- 1 to populationSize
      homeLocation = randomEdge
      workLocation = randomEdge
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
              homeEndTime
            ),
            Activity(
              ActivityType.Work,
              workLocation,
              workTime,
              workTime.plusHours(workDurationHours)
            ),
            TravelMode.Car
          ),
          AgentActivityPair(
            Activity(
              ActivityType.Work,
              workLocation,
              workTime,
              workTime.plusHours(workDurationHours)
            ),
            FinalActivity(
              ActivityType.Home,
              homeLocation
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

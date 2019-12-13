package edu.colorado.fitzgero.sotestbed.matsim.model.agent

import java.time.LocalTime

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.AgentActivity.{Activity, FinalActivity, FirstActivity}
import edu.colorado.fitzgero.sotestbed.model.agent.{RequestClass, TravelMode}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import org.matsim.api.core.v01.Coord

class AgentTest extends SoTestBedBaseTest {

  "AgentTest" should {
    "toXML" in {
      val agent = Agent(
        "test",
        RequestClass.UE,
        List(
          AgentActivityPair(
            FirstActivity(
              ActivityType.Home,
              EdgeId("e1"),
              new Coord(0,0),
              LocalTime.parse("08:00:00")
            ),
            Activity(
              ActivityType.Work,
              EdgeId("e2"),
              new Coord(10,10),
              LocalTime.parse("09:00:00"),
              LocalTime.parse("17:00:00")
            ),
            TravelMode.Car
          ),
          AgentActivityPair(
            Activity(
              ActivityType.Work,
              EdgeId("e2"),
              new Coord(10,10),
              LocalTime.parse("09:00:00"),
              LocalTime.parse("17:00:00")
            ),
            FinalActivity(
              ActivityType.Home,
              EdgeId("e1"),
              new Coord(0,0),
            ),
            TravelMode.Car
          )
        )
      )

      println(agent.toXML)

      // todo: test contents of toXML'd data
    }
  }
}

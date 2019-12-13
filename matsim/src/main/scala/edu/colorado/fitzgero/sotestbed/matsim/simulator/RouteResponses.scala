package edu.colorado.fitzgero.sotestbed.matsim.simulator

import edu.colorado.fitzgero.sotestbed.model.agent.Response

final case class RouteResponses(timeOfDay: Double, responses: List[Response])

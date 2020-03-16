package edu.colorado.fitzgero.sotestbed.model.agent

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

case class Request(agent: String, origin: EdgeId, destination: EdgeId, requestClass: RequestClass, travelMode: TravelMode, departureTime: SimTime)

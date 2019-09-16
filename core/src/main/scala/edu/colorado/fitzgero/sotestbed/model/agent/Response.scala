package edu.colorado.fitzgero.sotestbed.model.agent
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

case class Response(request: Request, path: List[EdgeId])

package edu.colorado.fitzgero.sotestbed.model.agent
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

case class Response(request: Request, pathIndex: Int, path: List[EdgeId], costEstimate: Cost)
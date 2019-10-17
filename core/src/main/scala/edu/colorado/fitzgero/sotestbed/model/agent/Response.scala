package edu.colorado.fitzgero.sotestbed.model.agent
import cats.Monad
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetwork}

case class Response(request: Request, path: List[EdgeId])

object Response {
  /**
    * turns a response into its MATSim XML representation
    * @param graph    underlying graph structure
    * @param response response data
    * @return response in xml format
    */
  def generateXML[F[_]: Monad, V, E](graph: RoadNetwork[F, V, E], response: Response): xml.Elem = {
//    val (src: String, dst: String) =
//      if (response.path.isEmpty) {
//        println("[LocalPopulationOps:generateXML] a response with an empty path!")
//        ("","")
//      }
//      else if (response.path.size == 1) {
//        (response.path.head.edgeId, response.path.head.edgeId)
//      } else {
//        (response.path.head.edgeId, response.path.last.edgeId)
//      }
//
//    <person id={response.request.id}>
//      <plan selected="yes">
//        <activity type="home" link={src} end_time={response.request.requestTime.format(HHmmssFormat)}/>
//        <leg mode="car">
//          <route type="links" start_link={src} end_link={dst}>
//            {response.path.map(_.edgeId).mkString(" ")}
//          </route>
//        </leg>
//        <activity type="work" link={dst} end_time={response.request.requestTime.plusHours(9).toString}/>
//      </plan>
//    </person>
  ???
  }
}
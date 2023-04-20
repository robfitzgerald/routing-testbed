package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population

import java.time.LocalTime

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.util.Random

import edu.colorado.fitzgero.sotestbed.matsim.model.agent.AgentActivity.{Activity, FinalActivity, FirstActivity}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.{ActivityType, Agent, AgentActivityPair}
import edu.colorado.fitzgero.sotestbed.model.agent.{RequestClass, TravelMode}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, Point, PrecisionModel}
import org.matsim.api.core.v01.network.{Link, Network}
import org.matsim.api.core.v01.{Coord, Id}
import org.matsim.core.network.NetworkUtils
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.opengis.referencing.operation.MathTransform
import com.typesafe.scalalogging.LazyLogging

case class UniformPolygonPopulationSamplingAlgorithm(
  mapBoundingGeometry: Geometry,
  boundingGeometrySRID: Int,
  networkSRID: Int,
  matsimNetwork: Network,
  populationSize: Int,
//  percentSOAgents: Double,
  workActivityMinTime: LocalTime,
  workActivityMaxTime: LocalTime,
  workDurationHours: Int,
  seedOption: Option[Long]
) extends PopSamplingAlgorithm
    with LazyLogging {

  val random: Random = seedOption match {
    case Some(seed) => new Random(seed)
    case None       => Random
  }

  def generate: List[Agent] = {

    val links: Map[Id[Link], Link]           = matsimNetwork.getLinks.asScala.toMap
    val geometryFactory: GeometryFactory     = new GeometryFactory(new PrecisionModel(), boundingGeometrySRID)
    val sourceCRS: CoordinateReferenceSystem = CRS.decode(s"EPSG:$boundingGeometrySRID");
    val targetCRS: CoordinateReferenceSystem = CRS.decode(s"EPSG:$networkSRID");

    val transform: MathTransform = CRS.findMathTransform(sourceCRS, targetCRS);

    @tailrec
    def randomEdge(ignoreEdge: Option[EdgeId] = None): EdgeId = {

      val result: Either[Exception, EdgeId] = for {
        point <- UniformPolygonPopulationSamplingAlgorithm.samplePointInGeometry(
          mapBoundingGeometry,
          geometryFactory,
          random
        )
        pointTransformed = JTS.transform(point, transform)
        nearestLink = NetworkUtils.getNearestLink(
          matsimNetwork,
          new Coord(pointTransformed.getCoordinate.getX, pointTransformed.getCoordinate.getY)
        )
      } yield {
        EdgeId(nearestLink.getId.toString)
      }

      result match {
        case Left(e) =>
          throw e
        case Right(linkId) if ignoreEdge.exists(_ == linkId) => randomEdge(ignoreEdge)
        case Right(linkId)                                   => linkId
      }
    }

    val secondsBetweenMinAndMaxWorkTime: Int =
      workActivityMaxTime.minusSeconds(workActivityMinTime.toSecondOfDay).toSecondOfDay
    def sampleWorkTime: LocalTime = workActivityMinTime.plusSeconds(random.nextInt(secondsBetweenMinAndMaxWorkTime))
//    def isSoAgent: Boolean                   = random.nextDouble < percentSOAgents

    val agents: Seq[Agent] = for {
      uniqueId <- 1 to populationSize
      homeLocation = randomEdge()
      homeNode <- links.get(Id.createLinkId(homeLocation.value))
      homeCoord    = homeNode.getCoord
      workLocation = randomEdge(ignoreEdge = Some(homeLocation))
      workNode <- links.get(Id.createLinkId(workLocation.value))
      workCoord = workNode.getCoord
      agentId   = s"$uniqueId-$homeLocation-$workLocation"
//      requestClass = if (isSoAgent) RequestClass.SO() else RequestClass.UE
      workTime    = sampleWorkTime
      homeEndTime = if (workTime.isAfter(LocalTime.parse("01:00:00"))) workTime.minusHours(1) else LocalTime.MIN
    } yield {

      val agent = Agent(
        agentId,
//        requestClass,
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

object UniformPolygonPopulationSamplingAlgorithm {

  /**
    * randomly samples a point from within a geometry by creating random points within it's bounding
    * box until one such point also intersects with the geometry
    * @param geometry the geometry to sample from
    * @param geometryFactory the factory used to create a point (with a CRS)
    * @param random the random number generator used for sampling
    * @param retryLimit the number of times to attempt, default 1000.
    * @return a point sampled from the geometry, or, an error. the error can be caused when a bounding
    *         box for the geometry has an area which is much greater than the area of the geometry itself,
    *         for example, a slim, diagonal areal region.
    */
  def samplePointInGeometry(
    geometry: Geometry,
    geometryFactory: GeometryFactory,
    random: Random,
    retryLimit: Int = 1000
  ): Either[Exception, Point] = {

    val (minx, maxx, miny, maxy) = (
      geometry.getEnvelopeInternal.getMinX,
      geometry.getEnvelopeInternal.getMaxX,
      geometry.getEnvelopeInternal.getMinY,
      geometry.getEnvelopeInternal.getMaxY
    )

    @tailrec
    def _sample(tries: Int = 0): Either[Exception, Point] = {
      if (tries >= retryLimit) {
        Left(new Exception(s"failed to sample point in provided bounding geometry after $tries tries"))
      } else {
        val (nextX, nextY) = (
          (random.nextDouble * (maxx - minx)) + minx,
          (random.nextDouble * (maxy - miny)) + miny
        )
        val point: Point = geometryFactory.createPoint(new org.locationtech.jts.geom.Coordinate(nextX, nextY))
        if (geometry.contains(point)) {
          Right(point)
        } else {
          _sample(tries + 1)
        }
      }
    }

    _sample()
  }
}

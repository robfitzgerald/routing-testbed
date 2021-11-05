package edu.colorado.fitzgero.sotestbed.algorithm.batching

import scala.util.Try

import cats.Monad
import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

case class GreedyBatching(
  maxBatchSize: Int,
  maxBatchRadius: Double
) extends BatchingFunction
    with LazyLogging {

  /**
    * takes the current batching strategy and any updates about replan-able agents, and spits out an
    * update to that batching strategy
    *
    * @param roadNetwork         the current road network state
    * @param activeRouteRequests agents which are available for SO routing requests
    * @param currentTime         the current sim time
    * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
    */
  def updateBatchingStrategy(
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    activeRouteRequests: List[AgentBatchData.RouteRequestData],
    currentTime: SimTime
  ): IO[Option[List[(String, List[Request])]]] = {

    val requestsWithCoordsIO = activeRouteRequests.traverse { d =>
      for {
        srcVertexId <- roadNetwork.source(d.request.origin)
        srcVertex   <- srcVertexId.traverse { roadNetwork.vertex }
        coord       <- IO.fromEither(Either.fromOption(srcVertex.flatten.map { _.attribute }, new Error("no vertex found")))
      } yield {
        (coord, d.request)
      }
    }

    val reqPairsSortedAscendingIO = for {
      reqsWithCoords <- requestsWithCoordsIO
    } yield {
      reqsWithCoords
        .combinations(2)
        .toList
        .sortBy {
          case (aCoord, _) :: (bCoord, _) :: Nil =>
            val dist = GreedyBatching.euclideanDistance(aCoord, bCoord)
            dist
        }
        .takeWhile {
          case (aCoord, _) :: (bCoord, _) :: Nil =>
            GreedyBatching.euclideanDistance(aCoord, bCoord) < this.maxBatchRadius
        }
    }

    val resultIO: IO[Option[List[(String, List[Request])]]] = for {
      reqsWithCoords          <- requestsWithCoordsIO
      reqPairsSortedAscending <- reqPairsSortedAscendingIO
    } yield {

      // agglomerate: each request begins as a singleton cluster
      val initialClusterAccumulator = GreedyBatching.ClusterAccumulator(maxBatchSize, maxBatchRadius)

      val clusters = reqPairsSortedAscending.foldLeft(initialClusterAccumulator) {
        case (acc, (aCoord, aReq) :: (bCoord, bReq) :: Nil) =>
          acc.getTag(aReq) match {
            case None =>
              acc.getTag(bReq) match {
                case None =>
                  // neither requests have a cluster => start a new cluster
                  acc.createCluster(aReq, aCoord, bReq, bCoord)
                case Some(bClusterId) =>
                  // request b has a cluster but a does not => propagate cluster
                  acc.attemptClusterPropagation(bClusterId, aReq, aCoord)
              }
            case Some(aClusterId) =>
              acc.getTag(bReq) match {
                case None =>
                  // request a has a cluster but b does not => propagate cluster
                  acc.attemptClusterPropagation(aClusterId, bReq, bCoord)
                case Some(bClusterId) =>
                  // both are tagged. can we merge? (test maxBatchSize + maxBatchRadius of resulting cluster)
                  acc.attemptMergeClusters(aClusterId, bClusterId)
              }
          }

      }

      val result = clusters.clusters.map {
        case (clusterId, cluster) =>
          (clusterId.toString, cluster.members.keys.toList)
      }.toList

      Some(result)
    }

    resultIO
  }
}

object GreedyBatching extends LazyLogging {

  final case class Cluster(centroid: Coordinate, members: Map[Request, Coordinate]) {

    def +(that: Cluster): Cluster = {
      val updatedMembers  = this.members ++ that.members
      val updatedCentroid = Cluster.centroid(updatedMembers.values)
      Cluster(updatedCentroid, updatedMembers)
    }

    def addToCluster(request: Request, coordinate: Coordinate): Cluster = {
      val updatedMembers = this.members.updated(request, coordinate)
      val newCentroid    = Cluster.centroid(updatedMembers.values)
      Cluster(newCentroid, updatedMembers)
    }

    def radius: Double = {
      members.foldLeft(0.0) {
        case (maxRad, (_, coord)) =>
          val thisRad = euclideanDistance(this.centroid, coord)
          math.max(maxRad, thisRad)
      }
    }
  }

  object Cluster {

    def apply(firstReq: Request, firstCoord: Coordinate): Cluster = {
      Cluster(centroid = firstCoord, Map(firstReq -> firstCoord))
    }

    def centroid(coords: Iterable[Coordinate]): Coordinate = {
      if (coords.isEmpty) throw new IllegalArgumentException("cannot find centroid of empty coordinate list")
      else {
        val (xSum, ySum, count) = coords.foldLeft((0.0, 0.0, 0)) { (acc, coord) =>
          val (xAcc, yAcc, count) = acc
          (xAcc + coord.x, yAcc + coord.y, count + 1)
        }
        Coordinate(xSum / count, ySum / count)
      }
    }
  }

  final case class ClusterAccumulator(
    maxClusterSize: Int,
    maxClusterRadius: Double,
    tags: Map[Request, Int] = Map.empty,
    clusters: Map[Int, Cluster] = Map.empty,
    nextClusterId: Int = 0
  ) {
    def getTag(request: Request): Option[Int] = this.tags.get(request)
    def isTagged(request: Request): Boolean   = this.tags.isDefinedAt(request)

    def createCluster(aReq: Request, aCoord: Coordinate, bReq: Request, bCoord: Coordinate): ClusterAccumulator = {
      val cluster         = Cluster(aReq, aCoord).addToCluster(bReq, bCoord)
      val updatedTags     = this.tags.updated(aReq, nextClusterId).updated(bReq, nextClusterId)
      val updatedClusters = this.clusters.updated(nextClusterId, cluster)
      this.copy(
        tags = updatedTags,
        clusters = updatedClusters,
        nextClusterId = this.nextClusterId + 1
      )
    }

    def attemptClusterPropagation(clusterId: Int, dst: Request, dstCoordinate: Coordinate): ClusterAccumulator = {
      Try {
        this.clusters(clusterId)
      }.toEither match {
        case Left(error) =>
          logger.error(s"cluster id exists but no matching cluster entry", error)
          this
        case Right(cluster) =>
          if (cluster.members.size >= this.maxClusterSize) {
            this
          } else {
            val updatedCluster = cluster.addToCluster(dst, dstCoordinate)
            if (cluster.radius >= maxClusterRadius) {
              this
            } else {
              val updatedTags     = this.tags.updated(dst, clusterId)
              val updatedClusters = this.clusters.updated(clusterId, updatedCluster)
              this.copy(
                tags = updatedTags,
                clusters = updatedClusters
              )
            }
          }
      }
    }

    def attemptMergeClusters(aId: Int, bId: Int): ClusterAccumulator = {
      val result: Option[ClusterAccumulator] = for {
        aCluster <- this.clusters.get(aId)
        bCluster <- this.clusters.get(bId)
      } yield {
        val combined: Cluster = aCluster + bCluster
        if (combined.members.size >= this.maxClusterSize || combined.radius >= this.maxClusterRadius) {
          this
        } else {
          // combine clusters
          val updatedTags: Map[Request, Int] = combined.members.keys.foldLeft(this.tags) { (t, req) =>
            t.updated(req, this.nextClusterId)
          }
          val updatedClusters: Map[Int, Cluster] = ((this.clusters - aId) - bId).updated(this.nextClusterId, combined)

          val result: ClusterAccumulator = this.copy(
            tags = updatedTags,
            clusters = updatedClusters,
            nextClusterId = this.nextClusterId + 1 // never repeat a cluster id, could cause issues
          )
          result
        }
      }

      result.getOrElse(this)
    }
  }

  def euclideanDistance(a: Coordinate, b: Coordinate): Double = {
    math.sqrt(math.pow(a.x - b.x, 2) + math.pow(a.y - b.y, 2))
  }

}

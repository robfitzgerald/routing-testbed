package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population

import org.locationtech.jts.index.strtree.STRtree
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.PopSampling
import edu.colorado.fitzgero.sotestbed.matsim.app.PopulationSamplingOps
import org.locationtech.jts.geom.Geometry
import scala.util.Try
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.Agent
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetworkIO

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.geom.Coordinate
import scala.collection.JavaConverters._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import java.time.LocalTime
import scala.util.Random
import edu.colorado.fitzgero.sotestbed.util.WeightedSamplingWithReplacement
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal
import com.typesafe.scalalogging.LazyLogging

/**
  * based on sampling trips from the DRCoG Focus model.
  * an input file provides zone x zone demand for some set of geometry zones.
  * each row has src/dst zone, departure time window, and count of trips.
  *
  * if the user specifies unweighted sampling, then, for each trip count on each row, a trip
  * is generated in MATSim.
  *
  * if the user specifies weighted sampling, they provide a targetPopulationSize. the counts
  * are converted into weights as ${row.count} / ${sum(rows)}. random weighted sampling generates
  * the $targetPopulationSize number of MATSim trips.
  */
sealed trait DemandTablePopSampling extends PopSamplingAlgorithm

object DemandTablePopSampling extends LazyLogging {

  final case class UnweightedSampling(
    strTree: STRtree,
    geoms: Map[String, Geometry],
    table: List[DemandTableRow],
    rn: RoadNetworkIO,
    seed: Int
  ) extends DemandTablePopSampling {

    def generate: Either[Error, List[Agent]] =
      generateAgentsFromExactTableCounts(this.strTree, this.table, this.rn, this.seed)
  }

  final case class WeightedSampling(
    targetPopulationSize: Int,
    strTree: STRtree,
    geoms: Map[String, Geometry],
    table: List[(DemandTableRow, Double)],
    rn: RoadNetworkIO,
    seed: Int
  ) extends DemandTablePopSampling {

    def generate: Either[Error, List[Agent]] =
      generateAgentsViaWeightedSampling(this.targetPopulationSize, this.strTree, this.table, this.rn, this.seed)
  }

  /**
    * builds a demand table population sampling algorithm based on the provided
    * input configurations.
    *
    * @param pop the input configurations
    */
  def build(pop: PopSampling.DemandSamplingTableInput, rn: RoadNetworkIO): Either[Error, DemandTablePopSampling] = {
    logger.info("building demand sampling table input with parameters:")
    logger.info(pop.toString)
    for {
      geoms <- PopSamplingFileOps.readWktCsv(
        file = pop.geometriesFile,
        srid = pop.geometriesFileSrid,
        idCol = pop.geometriesFileIdFieldName,
        geomCol = pop.geometriesFileGeomFieldName,
        targetSrid = 3857
      )
      _ = logger.info(f"read ${geoms.length} rows from geometries file ${pop.geometriesFile}")
      // validZoneIds = geoms.map { case (k, _) => k }.toSet
      demand <- PopSamplingFileOps.readDemandTableCsv(
        pop.demandFile,
        pop.demandFileSrcIdFieldName,
        pop.demandFileDstIdFieldName,
        pop.demandFileStartTimeFieldName,
        pop.demandFileEndTimeFieldName,
        pop.demandFileCountFieldName,
        pop.demandFileSeparator
      )
      _ = logger.info(f"read ${demand.length} rows from demand table file ${pop.demandFile}")
      tree <- buildZoneIdSpatialIndex(geoms)
      samp <- buildFromGeometriesAndDemandTable(tree, geoms, demand, rn, pop.targetPopulationSize, pop.seed)
      _ = logger.info("finished building demand sampling algorithm")
    } yield samp
  }

  /**
    * for each count on each row, sample an agent
    */
  def generateAgentsFromExactTableCounts(
    tree: STRtree,
    table: List[DemandTableRow],
    rn: RoadNetworkIO,
    seed: Long
  ): Either[Error, List[Agent]] = {
    val rng = new Random(seed)
    for {
      lookup <- buildEdgeIdsByZoneLookup(tree, rn)
    } yield for {
      row   <- table
      i     <- (0 until row.cnt).toList
      agent <- agentFromRow(i, row, rng, lookup)
    } yield agent
  }

  /**
    * use normalized counts per row as a weight and perform weighted sampling
    */
  def generateAgentsViaWeightedSampling(
    k: Int,
    tree: STRtree,
    table: List[(DemandTableRow, Double)],
    rn: RoadNetworkIO,
    seed: Long
  ): Either[Error, List[Agent]] = {
    val rng  = new Random(seed)
    val rows = WeightedSamplingWithReplacement.run(rng, table, k).toList
    for {
      lookup <- buildEdgeIdsByZoneLookup(tree, rn)
      agents = rows.zipWithIndex.flatMap { case (row, idx) => agentFromRow(idx, row, rng, lookup) }
    } yield agents
  }

  def agentFromRow(
    sampleId: Int,
    row: DemandTableRow,
    rng: Random,
    lookup: Map[String, Vector[EdgeId]]
  ): Option[Agent] = {
    val lookupResult = (lookup.get(row.src), lookup.get(row.dst))
    lookupResult match {
      case (Some(srcEdges), Some(dstEdges)) =>
        // pick a random time in the provided range
        val startSec = row.start.toSecondOfDay
        val endSec   = row.end.toSecondOfDay
        val randSec = // time bins may wrap over midnight
          if (startSec < endSec) rng.between(startSec, endSec)
          else rng.between(startSec, 86400 + endSec) % 86400

        // pick random start/end locations
        val srcLoc = srcEdges(rng.nextInt(srcEdges.length))
        val dstLoc = dstEdges(rng.nextInt(dstEdges.length))

        val randTime = LocalTime.ofSecondOfDay(randSec)
        val agentId  = s"${row.src}-${row.dst}-$sampleId"
        val src      = rng.nextInt(srcEdges.length)
        val dst      = rng.nextInt(dstEdges.length)

        val agent = Agent.singleTripAgent(agentId, srcLoc, dstLoc, randTime)

        Some(agent)
      case _ =>
        None // src or dst zone didn't intersect with road network
    }
  }

  def buildEdgeIdsByZoneLookup(
    tree: STRtree,
    rn: RoadNetworkIO
  ): Either[Error, Map[String, Vector[EdgeId]]] = {
    val MaxSpeedMph = 55.0
    val MaxSpeedMps = MaxSpeedMph * 0.446944444444444
    val gf          = new GeometryFactory(new PrecisionModel(), 3857)
    val edgesByZoneIOResult = rn.edgeTriplets
      .flatMap {
        _.traverse {
          case RoadNetwork.EdgeTriplet(src, edgeId, dst, edgeAttr) =>
            // ignore highway links here
            if (edgeAttr.freeFlowSpeed.value > MaxSpeedMps) IO.pure(None)
            else
              for {
                srcVtxOpt <- rn.vertex(src)
                srcVtx <- IO.fromOption(srcVtxOpt)(
                  new Error(s"internal error, source vertex $src of edge $edgeId not found in graph")
                )
                dstVtxOpt <- rn.vertex(dst)
                dstVtx <- IO.fromOption(dstVtxOpt)(
                  new Error(s"internal error, dest vertex $dst of edge $edgeId not found in graph")
                )
                zoneOpt <- zoneForLinkMidpoint(tree, srcVtx.attribute, dstVtx.attribute, gf)
              } yield zoneOpt.map { zone => (edgeId, zone) }
        }
      }
      .map {
        _.flatten
          .groupBy { case (edgeId, zone) => zone }
          .filter { case (zone, edges) => edges.nonEmpty }
          .map { case (zone, edges) => (zone, edges.map { case (edgeId, _) => edgeId }.toVector) }
      }
    val result = Try { edgesByZoneIOResult.unsafeRunSync }.toEither.left
      .map { t => new Error("internal error", t) }

    result
  }

  /**
    *
    */
  private[population] def zoneForLinkMidpoint(
    tree: STRtree,
    srcVtx: LocalAdjacencyListFlowNetwork.Coordinate,
    dstVtx: LocalAdjacencyListFlowNetwork.Coordinate,
    gf: GeometryFactory
  ): IO[Option[String]] = {
    val midX = (srcVtx.x + dstVtx.x) / 2.0
    val midY = (srcVtx.y + dstVtx.y) / 2.0
    for {
      midPt       <- IO.fromTry(Try { gf.createPoint(new Coordinate(midX, midY)) })
      queryResult <- IO.fromTry(Try { tree.query(midPt.getEnvelopeInternal) })
      zones       <- IO.fromTry(Try { queryResult.asScala.toList.map { _.asInstanceOf[String] } })
    } yield zones match {
      case Nil        => None
      case first :: _ => Some(first)
    }
  }

  private[population] def buildFromGeometriesAndDemandTable(
    tree: STRtree,
    geoms: List[(String, Geometry)],
    demand: List[DemandTableRow],
    rn: RoadNetworkIO,
    targetPopulationSize: Option[Int],
    seed: Option[Int]
  ): Either[Error, DemandTablePopSampling] = {
    val seedValue = seed.getOrElse(0)
    targetPopulationSize match {
      case None =>
        Right(UnweightedSampling(tree, geoms.toMap, demand, rn, seedValue))
      case Some(target) =>
        addTableWeights(demand).map { wDemand => WeightedSampling(target, tree, geoms.toMap, wDemand, rn, seedValue) }
    }
  }

  private[population] def addTableWeights(rows: List[DemandTableRow]): Either[Error, List[(DemandTableRow, Double)]] = {
    if (rows.isEmpty) Left(new Error("attempting to add table weights to empty table"))
    else {
      val max    = rows.map(_.cnt.toDouble).max
      val result = rows.map { r => (r, r.cnt.toDouble / max) }
      Right(result)
    }
  }

  private[population] def buildZoneIdSpatialIndex(data: List[(String, Geometry)]): Either[Error, STRtree] = {
    Try {
      val tree = new STRtree()
      data.foreach { case (id, geometry) => tree.insert(geometry.getEnvelopeInternal, id) }
      tree
    }.toEither.left.map { t => new Error("failure building zone id spatial index", t) }
  }

}

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

sealed trait DemandTablePopSampling extends PopSamplingAlgorithm

object DemandTablePopSampling {

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
    for {
      geoms <- PopSamplingFileOps.readWktCsv(
        pop.geometriesFile,
        pop.geometriesFileSrid,
        pop.geometriesFileIdFieldName,
        pop.geometriesFileGeomFieldName,
        targetSrid = 3857
      )
      demand <- PopSamplingFileOps.readDemandTableCsv(
        pop.demandFile,
        pop.demandFileSrcIdFieldName,
        pop.demandFileDstIdFieldName,
        pop.demandFileStartTimeFieldName,
        pop.demandFileEndTimeFieldName,
        pop.demandFileCountFieldName,
        pop.demandFileSeparator
      )
      tree <- buildZoneIdSpatialIndex(geoms)
      samp <- buildFromGeometriesAndDemandTable(tree, geoms, demand, rn, pop.targetPopulationSize)
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
      agents <- table.traverse { row => (0 until row.cnt).toList.traverse { i => agentFromRow(i, row, rng, lookup) } }
    } yield agents.flatten
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
      agents <- rows.zipWithIndex.traverse { case (row, idx) => agentFromRow(idx, row, rng, lookup) }
    } yield agents
  }

  def agentFromRow(
    sampleId: Int,
    row: DemandTableRow,
    rng: Random,
    lookup: Map[String, Vector[EdgeId]]
  ): Either[Error, Agent] = {
    for {
      srcEdges <- lookup.get(row.src).toRight(new Error("internal error"))
      dstEdges <- lookup.get(row.dst).toRight(new Error("internal error"))
    } yield {
      // pick a random time in the provided range
      val startSec = row.start.toSecondOfDay
      val endSec   = row.end.toSecondOfDay
      val randSec  = rng.between(startSec, endSec)

      // pick random start/end locations
      val srcLoc = srcEdges(rng.nextInt(srcEdges.length))
      val dstLoc = dstEdges(rng.nextInt(dstEdges.length))

      val randTime = LocalTime.ofSecondOfDay(randSec)
      val agentId  = s"${row.src}-${row.dst}-$sampleId"
      val src      = rng.nextInt(srcEdges.length)
      val dst      = rng.nextInt(dstEdges.length)

      val agent = Agent.singleTripAgent(agentId, srcLoc, dstLoc, randTime)

      agent
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
                srcVtx    <- IO.fromOption(srcVtxOpt)(new Error("internal error"))
                dstVtxOpt <- rn.vertex(dst)
                dstVtx    <- IO.fromOption(dstVtxOpt)(new Error("internal error"))
                zoneOpt   <- zoneForLinkMidpoint(tree, srcVtx.attribute, dstVtx.attribute, gf)
              } yield zoneOpt.map { zone => (edgeId, zone) }
        }
      }
      .map {
        _.flatten
          .groupBy { case (edgeId, zone) => zone }
          .filter { case (zone, edges) => edges.nonEmpty }
          .map { case (zone, edges) => (zone, edges.map { case (edgeId, _) => edgeId }.toVector) }
      }
    Try { edgesByZoneIOResult.unsafeRunSync }.toEither.left
      .map { t => new Error("internal error") }
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
    targetPopulationSize: Option[Int]
  ): Either[Error, DemandTablePopSampling] = {
    targetPopulationSize match {
      case None =>
        Right(UnweightedSampling(tree, geoms.toMap, demand, rn, 0))
      case Some(target) =>
        addTableWeights(demand).map { wDemand => WeightedSampling(target, tree, geoms.toMap, wDemand, rn, 0) }
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

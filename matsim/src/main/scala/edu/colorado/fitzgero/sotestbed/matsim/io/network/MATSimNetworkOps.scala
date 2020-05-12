package edu.colorado.fitzgero.sotestbed.matsim.io.network

import java.nio.file.Path

import scala.collection.JavaConverters._
import scala.util.Try

import org.matsim.api.core.v01.network.Network
import kantan.csv._
import kantan.csv.ops._

object MATSimNetworkOps {

  final case class NetworkStats(totalLengths: Double, speeds: Map[Double, Int]) {
    override def toString: String =
      f"""
         |total link lengths: $totalLengths%.2f
         |speed data:
         |${speeds.mkString("  ", ",\n  ", "")}
         |""".stripMargin
  }

  def networkStats(network: Network): NetworkStats = {
    val totalLengths =
      network.getLinks.asScala.map { _._2.getLength }.sum
    val speeds: Map[Double, Int] =
      network.getLinks.asScala.toMap
        .groupBy { _._2.getFreespeed }
        .map { case (speed, links) => (speed, links.size) }
    NetworkStats(totalLengths, speeds)
  }

  final case class CountOfCommonEdges(commonEdges: Int, edgesOnlyInA: Int, edgesOnlyInB: Int, lengthDiff: Double, freespeedDiff: Double) {
    override def toString: String =
      f"""
         |common links: $commonEdges
         |only in network a: $edgesOnlyInA
         |only in network b: $edgesOnlyInB
         |link length diff from a to b: $lengthDiff%.2f (avg ${lengthDiff / commonEdges}%.2f)
         |link freespeed diff from a to b: $freespeedDiff%.2f (avg ${freespeedDiff / commonEdges}%.2f)
         |""".stripMargin
  }

  /**
    *
    * @param a
    * @param b
    * @param roundingFn a function to round the coordinates, by default rounding to nearest integer
    */
  def countCommonEdgesByCommonCoordinates(
    a: Network,
    b: Network,
    roundingFn: Double => Double = (d: Double) => math.round(d).toDouble
  ): CountOfCommonEdges = {
    val aLinks = {
      for {
        (_, link) <- a.getLinks.asScala.toMap
      } yield {
        val (srcX, srcY) = (link.getFromNode.getCoord.getX, link.getFromNode.getCoord.getY)
        val (dstX, dstY) = (link.getToNode.getCoord.getX, link.getToNode.getCoord.getY)
        val result       = f"(${roundingFn(srcX)},${roundingFn(srcY)})(${roundingFn(dstX)},${roundingFn(dstY)})"
        (result, link.getId)
      }
    }
    val bLinks = {
      for {
        (_, link) <- b.getLinks.asScala.toMap
      } yield {
        val (srcX, srcY) = (link.getFromNode.getCoord.getX, link.getFromNode.getCoord.getY)
        val (dstX, dstY) = (link.getToNode.getCoord.getX, link.getToNode.getCoord.getY)
        val result       = f"(${roundingFn(srcX)},${roundingFn(srcY)})(${roundingFn(dstX)},${roundingFn(dstY)})"
        (result, link.getId)
      }
    }
    val commonLinks = aLinks.keySet.intersect(bLinks.keySet)
    val commonLinkDiffs = for {
      commonLink <- commonLinks
      aLinkId    <- aLinks.get(commonLink)
      bLinkId    <- bLinks.get(commonLink)
      aLink      <- a.getLinks.asScala.get(aLinkId)
      bLink      <- b.getLinks.asScala.get(bLinkId)
    } yield (aLink.getLength - bLink.getLength, aLink.getFreespeed - bLink.getFreespeed)
    val (lengthDiffs, freespeedDiffs) = commonLinkDiffs.unzip
    val lengthDiff                    = if (lengthDiffs.nonEmpty) lengthDiffs.sum else 0.0
    val freespeedDiff                 = if (freespeedDiffs.nonEmpty) freespeedDiffs.sum else 0.0
    val linksInAOnly                  = aLinks.size - commonLinks.size
    val linksInBOnly                  = bLinks.size - commonLinks.size

    CountOfCommonEdges(commonLinks.size, linksInAOnly, linksInBOnly, lengthDiff, freespeedDiff)
  }

  def createDiffWKTNetwork(
    a: Network,
    b: Network,
    outFile: Path,
    roundingFn: Double => Double = (d: Double) => math.round(d).toDouble
  ): Either[Exception, Unit] = {
    val aLinks = {
      for {
        (_, link) <- a.getLinks.asScala.toMap
      } yield {
        val (srcX, srcY) = (link.getFromNode.getCoord.getX, link.getFromNode.getCoord.getY)
        val (dstX, dstY) = (link.getToNode.getCoord.getX, link.getToNode.getCoord.getY)
        val result       = f"LINESTRING (${roundingFn(srcX)} ${roundingFn(srcY)}, ${roundingFn(dstX)} ${roundingFn(dstY)})"
        (result, link)
      }
    }
    val bLinks = {
      for {
        (_, link) <- b.getLinks.asScala.toMap
      } yield {
        val (srcX, srcY) = (link.getFromNode.getCoord.getX, link.getFromNode.getCoord.getY)
        val (dstX, dstY) = (link.getToNode.getCoord.getX, link.getToNode.getCoord.getY)
        val result       = f"LINESTRING (${roundingFn(srcX)} ${roundingFn(srcY)}, ${roundingFn(dstX)} ${roundingFn(dstY)})"
        (result, link)
      }
    }
    val commonLinks = aLinks.keySet.intersect(bLinks.keySet)
    val aRowData = for {
      (str, link) <- aLinks
      if !commonLinks(str)
    } yield {
      (str, 0.0, 0.0, true, false, false)
    }
    val bRowData = for {
      (str, link) <- bLinks
      if !commonLinks(str)
    } yield {
      (str, 0.0, 0.0, false, true, false)
    }
    val diffRowData = for {
      commonLink <- commonLinks
      aLink      <- aLinks.get(commonLink)
      bLink      <- bLinks.get(commonLink)
    } yield (commonLink, aLink.getLength - bLink.getLength, aLink.getFreespeed - bLink.getFreespeed, false, false, true)

    val rowData = aRowData ++ bRowData ++ diffRowData

    val writer =
      outFile.asCsvWriter[(String, Double, Double, Boolean, Boolean, Boolean)](
        rfc.withHeader("WKT", "length_diff", "freespeed_diff", "network_a", "network_b", "both"))

    Try {
      for {
        link <- rowData
      } {
        writer.write(link)
      }
    }.toEither.left.map { t =>
      new Exception(t)
    }
  }

  def writeWKTNetwork(
    network: Network,
    outFile: Path,
  ): Either[Exception, Unit] = {
    val rowData = {
      for {
        (_, link) <- network.getLinks.asScala.toMap
      } yield {
        val (srcX, srcY) = (link.getFromNode.getCoord.getX, link.getFromNode.getCoord.getY)
        val (dstX, dstY) = (link.getToNode.getCoord.getX, link.getToNode.getCoord.getY)
        val result       = f"LINESTRING ($srcX $srcY, $dstX $dstY)"
        (result, link.getLength, link.getFreespeed)
      }
    }

    val writer =
      outFile.asCsvWriter[(String, Double, Double)](rfc.withHeader("WKT", "length", "freespeed"))

    Try {
      for {
        link <- rowData
      } {
        writer.write(link)
      }
    }.toEither.left.map { t =>
      new Exception(t)
    }
  }
}

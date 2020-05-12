package edu.colorado.fitzgero.sotestbed.matsim.app

import java.nio.file.Path

import scala.util.{Random, Try}

import org.matsim.core.network.NetworkUtils
import cats.implicits._

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.algorithm.search.DijkstraSearch
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.TraverseDirection
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPRCostOps
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork

object MonteCarloTravelTimeLowerBoundsApp
    extends CommandApp(
      name = "monte-carlo-lower-bounds",
      header = "approximate the free-flow average network travel time via monte carlo sampling",
      main = {
        val fileOpt: Opts[Path] =
          Opts.option[Path](long = "file", short = "f", help = "a MATSim network file")

        val nOpt: Opts[Int] =
          Opts.option[Int](long = "n", short = "n", help = "number of samples")

        val seedOpt: Opts[Long] =
          Opts.option[Long](long = "seed", short = "s", help = "random seed value").withDefault(System.currentTimeMillis)

        (fileOpt, nOpt, seedOpt).mapN {
          (network, n, seed) =>
            {
              val result = for {
                roadNetwork <- LocalAdjacencyListFlowNetwork.fromMATSimXML(network.toFile)
                dijkstrasSearch = DijkstraSearch.edgeOrientedShortestPath(roadNetwork, EdgeBPRCostOps.freeFlowCostFunction) _
                edgeIds         = roadNetwork.edges.keys.toArray
              } yield {
                val random: Random = new Random(seed)
                val sumOfTravelTimes = (1 to n).foldLeft(0.0) { (acc, i) =>
                  if (i % 10000 == 0) {
                    val currentAvg: Double = acc / i
                    println(f"running ${i}th sample with current average $currentAvg%.2f")
                  }
                  val src = edgeIds(random.nextInt(edgeIds.length))
                  val dst = edgeIds(random.nextInt(edgeIds.length))
                  dijkstrasSearch(src, dst, TraverseDirection.Forward).unsafeRunSync() match {
                    case None =>
                      acc
                    case Some(path) =>
                      val travelTime: Double = if (path.isEmpty) 0.0 else path.map { _.cost.value }.sum
                      acc + travelTime
                  }
                }
                val travelTimeEstimate = sumOfTravelTimes / n.toDouble
                travelTimeEstimate
              }

              result match {
                case Left(e) =>
                  println(s"failed due to $e")
                  ()
                case Right(result) =>
                  println(f"monte carlo travel time estimate after $n samples is $result%.2f")
                  ()
              }
            }
        }
      }
    )

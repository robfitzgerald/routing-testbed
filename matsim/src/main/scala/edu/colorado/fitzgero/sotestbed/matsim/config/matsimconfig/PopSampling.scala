package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig

import java.time.LocalTime
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population._
import scala.util.Try
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import org.matsim.core.network.NetworkUtils
import edu.colorado.fitzgero.sotestbed.matsim.app.PopulationSamplingOps
import java.io.File

sealed trait PopSampling

object PopSampling {

  /**
    * build a population by sampling from a distribution of ODs by some
    * zonal level of aggregation.
    *
    * if a target population size is specified, the counts per row are
    * min/max normalized and used as sample weights. otherwise, for each
    * count, a new population entry is made.
    *
    * @param geometriesFile
    * @param demandFile
    * @param targetPopulationSize
    * @param geometriesFileIdFieldName
    * @param geometriesFileGeomFieldName
    * @param demandFileSrcIdFieldName
    * @param demandFileDstIdFieldName
    * @param demandFileStartTimeFieldName
    * @param demandFileEndTimeFieldName
    * @param demandFileCountFieldName
    * @param demandFileSeparator
    */
  final case class DemandSamplingTableInput(
    geometriesFile: File,
    demandFile: File,
    targetPopulationSize: Option[Int],
    geometriesFileIdFieldName: String = "id",
    geometriesFileGeomFieldName: String = "geometry",
    geometriesFileSrid: Int = 4326,
    demandFileSrcIdFieldName: String = "src",
    demandFileDstIdFieldName: String = "dst",
    demandFileStartTimeFieldName: String = "start_time",
    demandFileEndTimeFieldName: String = "end_time",
    demandFileCountFieldName: String = "count",
    demandFileSeparator: Char = ','
  ) extends PopSampling

  /**
    * build a population sampling algorithm that uniformly samples trips from edges in the road network
    *
    * @param workActivityMinTime
    * @param workActivityMaxTime
    * @param workDurationHours
    * @param seed
    */
  final case class UnifEdgeSingleTrip(
    workActivityMinTime: LocalTime = LocalTime.parse("08:30:00"),
    workActivityMaxTime: LocalTime = LocalTime.parse("09:30:00"),
    workDurationHours: Int = 8,
    seed: Option[Long] = None
  ) extends PopSampling

  /**
    * build a population sampling algorithm using uniform sampling over the graph edge set and using
    * a time range for the workplace activity. two trips are created for each agent in the population,
    * one headed to work in the morning, and one headed home in the evening.
    *
    * @param workActivityMinTime
    * @param workActivityMaxTime
    * @param workDurationHours
    * @param seed
    */
  final case class UniformPopLinkSampling(
    workActivityMinTime: LocalTime = LocalTime.parse("08:30:00"),
    workActivityMaxTime: LocalTime = LocalTime.parse("09:30:00"),
    workDurationHours: Int = 8,
    seed: Option[Long] = None
  ) extends PopSampling

  /**
    * in this variant a geofence polygon is provided to restrict sampling locations.
    *
    * build a population sampling algorithm using uniform sampling over the graph edge set and using
    * a time range for the workplace activity. two trips are created for each agent in the population,
    * one headed to work in the morning, and one headed home in the evening.
    *
    * @param geometryPath
    * @param geometrySRID
    * @param workActivityMinTime
    * @param workActivityMaxTime
    * @param workDurationHours
    * @param seed
    */
  final case class UniformPopPolygonSampling(
    geometryPath: File,
    geometrySRID: Int = 4326, // assumed to be WGS84
    networkSRID: Int = 3857,  // assumed to be web mercator
    workActivityMinTime: LocalTime = LocalTime.parse("08:30:00"),
    workActivityMaxTime: LocalTime = LocalTime.parse("09:30:00"),
    workDurationHours: Int = 8,
    seed: Option[Long] = None
  ) extends PopSampling

  implicit class PopSamplingImpl(p: PopSampling) {

    def build(matsimPopConfig: MATSimPopConfig): Either[Error, PopSamplingAlgorithm] =
      p match {
        case pop: DemandSamplingTableInput =>
          val result = for {
            rn   <- LocalAdjacencyListFlowNetwork.fromMATSimXML(matsimPopConfig.fs.matsimNetworkFile)
            samp <- DemandTablePopSampling.build(pop, rn)
          } yield samp

          result.left.map { s => new Error(s.toString) }

        case pop: UnifEdgeSingleTrip =>
          val result = for {
            roadNetwork   <- LocalAdjacencyListFlowNetwork.fromMATSimXML(matsimPopConfig.fs.matsimNetworkFile)
            matsimNetwork <- Try { NetworkUtils.readNetwork(matsimPopConfig.fs.matsimNetworkFile.toString) }.toEither
          } yield {
            UniformEdgePopSamplingSingleTrip(
              roadNetwork,
              matsimNetwork,
              matsimPopConfig.pop.size,
              //            matsimPopConfig.pop.adoptionRate,
              pop.workActivityMinTime,
              pop.workActivityMaxTime,
              pop.workDurationHours,
              pop.seed
            )
          }

          result.left.map { s => new Error(s.toString) }

        case pop: UniformPopLinkSampling =>
          val result = for {
            roadNetwork   <- LocalAdjacencyListFlowNetwork.fromMATSimXML(matsimPopConfig.fs.matsimNetworkFile)
            matsimNetwork <- Try { NetworkUtils.readNetwork(matsimPopConfig.fs.matsimNetworkFile.toString) }.toEither
          } yield {
            UniformEdgePopulationSamplingAlgorithm(
              roadNetwork,
              matsimNetwork,
              matsimPopConfig.pop.size,
              //            matsimPopConfig.pop.adoptionRate,
              pop.workActivityMinTime,
              pop.workActivityMaxTime,
              pop.workDurationHours,
              pop.seed
            )
          }

          result.left.map { s => new Error(s.toString) }

        case pop: UniformPopPolygonSampling =>
          val result = for {
            geometry      <- PopulationSamplingOps.readBoundingGeometryXYCsv(pop.geometryPath)
            matsimNetwork <- Try { NetworkUtils.readNetwork(matsimPopConfig.fs.matsimNetworkFile.toString) }.toEither
          } yield {
            UniformPolygonPopulationSamplingAlgorithm(
              geometry,
              boundingGeometrySRID = pop.geometrySRID,
              networkSRID = pop.networkSRID,
              matsimNetwork,
              matsimPopConfig.pop.size,
//            matsimPopConfig.pop.adoptionRate,
              pop.workActivityMinTime,
              pop.workActivityMaxTime,
              pop.workDurationHours,
              pop.seed
            )
          }

          result.left.map { s => new Error(s.toString) }

      }
  }
}

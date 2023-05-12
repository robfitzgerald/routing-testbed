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
    * @param matsimNetworkFile network file to build population on top of
    * @param targetPopulationSize
    * @param geometriesFileIdFieldName
    * @param geometriesFileGeomFieldName
    * @param demandFileSrcIdFieldName
    * @param demandFileDstIdFieldName
    * @param demandFileStartTimeFieldName
    * @param demandFileEndTimeFieldName
    * @param demandFileCountFieldName
    * @param demandFileSeparator
    * @param seed seed value for random sampling. default Zero
    */
  final case class DemandSamplingTableInput(
    geometriesFile: File,
    demandFile: File,
    matsimNetworkFile: File,
    targetPopulationSize: Option[Int],
    geometriesFileIdFieldName: String = "id",
    geometriesFileGeomFieldName: String = "geometry",
    geometriesFileSrid: Int = 4326,
    demandFileSrcIdFieldName: String = "src",
    demandFileDstIdFieldName: String = "dst",
    demandFileStartTimeFieldName: String = "start_time",
    demandFileEndTimeFieldName: String = "end_time",
    demandFileCountFieldName: String = "count",
    demandFileSeparator: Char = ',',
    seed: Option[Int] = None
  ) extends PopSampling

  /**
    * build a population sampling algorithm using uniform sampling over the graph edge set and using
    * a time range for the workplace activity. two trips are created for each agent in the population,
    * one headed to work in the morning, and one headed home in the evening.
    *
    * @param matsimNetworkFile network file to build population on top of
    * @param workActivityMinTime
    * @param workActivityMaxTime
    * @param workDurationHours
    * @param singleTrip if true, generate one trip per reified MATSim person (two "persons", one Home->Work, one Work->Home)
    * @param seed
    */
  final case class UniformPopLinkSampling(
    matsimNetworkFile: File,
    size: Int,
    workActivityMinTime: LocalTime = LocalTime.parse("08:30:00"),
    workActivityMaxTime: LocalTime = LocalTime.parse("09:30:00"),
    workDurationHours: Int = 8,
    singleTrip: Boolean = true,
    seed: Option[Long] = None
  ) extends PopSampling

  /**
    * in this variant a geofence polygon is provided to restrict sampling locations.
    *
    * build a population sampling algorithm using uniform sampling over the graph edge set and using
    * a time range for the workplace activity. two trips are created for each agent in the population,
    * one headed to work in the morning, and one headed home in the evening.
    *
    * @param matsimNetworkFile network file to build population on top of
    * @param geometryPath
    * @param geometrySRID
    * @param workActivityMinTime
    * @param workActivityMaxTime
    * @param workDurationHours
    * @param seed
    */
  final case class UniformPopPolygonSampling(
    geometryPath: File,
    matsimNetworkFile: File,
    size: Int,
    geometrySRID: Int = 4326, // assumed to be WGS84
    networkSRID: Int = 3857,  // assumed to be web mercator
    workActivityMinTime: LocalTime = LocalTime.parse("08:30:00"),
    workActivityMaxTime: LocalTime = LocalTime.parse("09:30:00"),
    workDurationHours: Int = 8,
    seed: Option[Long] = None
  ) extends PopSampling

  implicit class PopSamplingImpl(p: PopSampling) {

    def getSamplingAlgorithmName: String = p.getClass.getSimpleName

    def getSeed: Option[Int] = p match {
      case u: DemandSamplingTableInput  => u.seed.map(_.toInt)
      case u: UniformPopLinkSampling    => u.seed.map(_.toInt)
      case u: UniformPopPolygonSampling => u.seed.map(_.toInt)
    }

    def filename(iteration: Option[Int] = None, suffix: String = ".xml"): String = {
      val seedString = p.getSeed.map(_.toString).getOrElse("none")
      val sizeString = p match {
        case u: DemandSamplingTableInput  => u.targetPopulationSize.getOrElse("none")
        case u: UniformPopLinkSampling    => u.size.toString
        case u: UniformPopPolygonSampling => u.size.toString
      }
      f"population-$getSamplingAlgorithmName-i=$iteration-s=$seedString-k=$sizeString$suffix"
    }

    def description: String = {
      val seedString = p.getSeed.map(_.toString).getOrElse("none")
      val sizeString = p match {
        case u: DemandSamplingTableInput  => u.targetPopulationSize.map(_.toString).getOrElse("none")
        case u: UniformPopLinkSampling    => u.size.toString
        case u: UniformPopPolygonSampling => u.size.toString
      }
      f"""POPULATION INPUT:
         |algorithm: $getSamplingAlgorithmName
         |size argument: $sizeString
         |random seed: $seedString""".stripMargin
    }

    def build(): Either[Error, PopSamplingAlgorithm] =
      p match {
        case pop: DemandSamplingTableInput =>
          val result = for {
            rn   <- LocalAdjacencyListFlowNetwork.fromMATSimXML(pop.matsimNetworkFile).left.map { s => new Error(s) }
            samp <- DemandTablePopSampling.build(pop, rn)
          } yield samp

          result

        case pop: UniformPopLinkSampling =>
          val result = for {
            roadNetwork <- LocalAdjacencyListFlowNetwork.fromMATSimXML(pop.matsimNetworkFile).left.map { s =>
              new Error(s)
            }
            matsimNetwork <- Try { NetworkUtils.readNetwork(pop.matsimNetworkFile.toString) }.toEither
          } yield {
            if (pop.singleTrip) {
              UniformEdgePopSamplingSingleTrip(
                roadNetwork,
                matsimNetwork,
                pop.size,
                //            matsimPopConfig.pop.adoptionRate,
                pop.workActivityMinTime,
                pop.workActivityMaxTime,
                pop.workDurationHours,
                pop.seed
              )
            } else {
              UniformEdgePopulationSamplingAlgorithm(
                roadNetwork,
                matsimNetwork,
                pop.size,
                //            matsimPopConfig.pop.adoptionRate,
                pop.workActivityMinTime,
                pop.workActivityMaxTime,
                pop.workDurationHours,
                pop.seed
              )
            }

          }

          result.left.map { s => new Error(s) }

        case pop: UniformPopPolygonSampling =>
          val result = for {
            geometry      <- PopulationSamplingOps.readBoundingGeometryXYCsv(pop.geometryPath)
            matsimNetwork <- Try { NetworkUtils.readNetwork(pop.matsimNetworkFile.toString) }.toEither
          } yield {
            UniformPolygonPopulationSamplingAlgorithm(
              geometry,
              boundingGeometrySRID = pop.geometrySRID,
              networkSRID = pop.networkSRID,
              matsimNetwork,
              pop.size,
//            matsimPopConfig.pop.adoptionRate,
              pop.workActivityMinTime,
              pop.workActivityMaxTime,
              pop.workDurationHours,
              pop.seed
            )
          }

          result.left.map { s => new Error(s) }

      }
  }
}

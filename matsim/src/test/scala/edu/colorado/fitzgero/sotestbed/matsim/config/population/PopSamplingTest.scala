package edu.colorado.fitzgero.sotestbed.matsim.config.population

import java.io.File
import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.PopSampling
import java.nio.file.Paths
import java.nio.file.Files
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population.PopSamplingAlgorithm

class PopSamplingTest extends SoTestBedBaseTest {
  "pop sampling" when {
    "demand table pop sampling" when {
      "called with a valid input" when {
        "unweighted sampling" should {
          "create a sample with the same count as the input count" in {

            val resourceDir = Paths.get("matsim/src/test/resources/pop_sampling_demand_table/")

            val popSampling = PopSampling.DemandSamplingTableInput(
              geometriesFile = resourceDir.resolve("taz_epsg3857.csv").toFile,
              demandFile = resourceDir.resolve("demand_table.csv").toFile,
              matsimNetworkFile = resourceDir.resolve("matsim_network.xml").toFile,
              targetPopulationSize = None,
              geometriesFileIdFieldName = "gid",
              geometriesFileSrid = 3857,
              demandFileSrcIdFieldName = "src_zone_id",
              demandFileDstIdFieldName = "dst_zone_id"
            )

            // known total in the file demand_table.csv
            // >>> df = pd.read_csv("demand_table.csv")
            // >>> df['count'].sum()
            // 42067
            val sumOfDemandCountColumn = 42067

            val result = for {
              alg    <- popSampling.build()
              agents <- alg.generate
            } yield agents

            result match {
              case Left(value)  => throw value
              case Right(value) => value.length should equal(sumOfDemandCountColumn)
            }
          }
        }
        "weighted sampling with some k value" should {
          "create a sample with k rows" in {

            val resourceDir          = Paths.get("matsim/src/test/resources/pop_sampling_demand_table/")
            val targetPopulationSize = 1000
            val popSampling = PopSampling.DemandSamplingTableInput(
              geometriesFile = resourceDir.resolve("taz_epsg3857.csv").toFile,
              demandFile = resourceDir.resolve("demand_table.csv").toFile,
              matsimNetworkFile = resourceDir.resolve("matsim_network.xml").toFile,
              targetPopulationSize = Some(targetPopulationSize),
              geometriesFileIdFieldName = "gid",
              geometriesFileSrid = 3857,
              demandFileSrcIdFieldName = "src_zone_id",
              demandFileDstIdFieldName = "dst_zone_id"
            )

            val result = for {
              alg    <- popSampling.build()
              agents <- alg.generate
            } yield agents

            result match {
              case Left(value) => throw value
              case Right(value) =>
                value.length should equal(targetPopulationSize)
            }

          }
        }
      }
    }
  }
}

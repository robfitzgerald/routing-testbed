package edu.colorado.fitzgero.sotestbed.matsim.config.batch

import java.io.File
import java.nio.file.{Path, Paths}

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.config.BatchingFunctionConfig.Random
import edu.colorado.fitzgero.sotestbed.config.KSPAlgorithmConfig.SvpLoSync
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig

class MATSimBatchConfigTest extends SoTestBedBaseTest {

  "MATSimBatchConfigTest" should {
    "readBatchConfig" in {
      val defaultConfDir: Path = Paths.get("matsim/src/main/resources/test/")
      val batchConf            = new File("matsim/src/main/resources/test/BatchConfigTest.conf")

      MATSimBatchConfig.readBatchConfig(batchConf, defaultConfDir) match {
        case Left(e) => fail(e)
        case Right(result) =>
          for {
            MATSimBatchConfig.Variation(_, confResult, _, _) <- result
            conf                                             <- confResult
          } {
            List(15L, 30L) should contain(conf.routing.batchWindow.value)
            conf.algorithm match {
              case so: MATSimConfig.Algorithm.SystemOptimal =>
                so.name should equal("system optimal")
                so.batchingFunction match {
                  case greedy: Random => List(5, 10) should contain(greedy.batchWindow)
                  case _              => fail("should have found a Greedy batching algorithm config object")
                }
              case other => fail(f"didn't expect ${other.getClass.getSimpleName}")
            }
          }

          result.foreach(println)

          // two variations for 3 parameters should create 8 combinations
          result.size should equal(4)
      }
    }
  }
}

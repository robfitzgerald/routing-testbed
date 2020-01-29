package edu.colorado.fitzgero.sotestbed.matsim.config.batch

import java.io.File
import java.nio.file.{Path, Paths}

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.config.algorithm.BatchingFunctionConfig.Greedy
import edu.colorado.fitzgero.sotestbed.config.algorithm.KSPAlgorithmConfig.SvpLoSync
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig

class MATSimBatchConfigTest extends SoTestBedBaseTest {

  "MATSimBatchConfigTest" should {
    "readBatchConfig" in {
      val defaultConfDir: Path = Paths.get("matsim/src/main/resources/test/")
      val batchConf = new File("matsim/src/main/resources/test/BatchConfigTest.conf")

      MATSimBatchConfig.readBatchConfig(batchConf, defaultConfDir) match {
        case Left(e) => fail(e)
        case Right(result) =>
          for {
            MATSimBatchConfig.Variation(confResult, _, _) <- result
            conf <- confResult
          } {
            List(15L,30L) should contain (conf.routing.batchWindow.value)
            conf.algorithm match {
              case _: MATSimConfig.Algorithm.Selfish => fail("wrong algorithm type found")
              case so: MATSimConfig.Algorithm.SystemOptimal =>

                so.name should equal ("system optimal")
                so.batchingFunction match {
                  case greedy: Greedy => List(5,10) should contain (greedy.batchWindow)
                  case _ => fail("should have found a Greedy batching algorithm config object")
                }
            }
          }

          result.foreach(println)

          // two variations for 3 parameters should create 8 combinations
          result.size should equal (4)
      }
    }
  }
}

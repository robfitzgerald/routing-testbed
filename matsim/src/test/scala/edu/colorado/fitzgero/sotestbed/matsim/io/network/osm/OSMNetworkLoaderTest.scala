package edu.colorado.fitzgero.sotestbed.matsim.io.network.osm

import java.nio.file.{Path, Paths}

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest

class OSMNetworkLoaderTest extends SoTestBedBaseTest {

  "OSMNetworkLoaderTest" should {

    "createOSMNetworkWithUTMTransformation" in {
      val src: Path      = Paths.get("/Users/robertfitzgerald/dev/ucd/phd/projects/2020sp/so-testbed-route-viz/20200410_batch/Rye_NY.xml")
      val dstCRS: String = "EPSG:32618"

      OSMNetworkLoader.createOSMNetworkWithUTMTransformation(src, dstCRS) match {
        case Left(e) =>
          fail(e)
        case Right(n) =>
          succeed
      }
    }

  }
}

package edu.colorado.fitzgero.sotestbed

import java.io.File

import org.scalatest.{Matchers, WordSpec}

abstract class SoTestBedBaseTest extends WordSpec with Matchers {

  object TestNetwork {
    def fiveByFiveNetworkFile: File        = new File("matsim/src/main/resources/matsim/network/test-network/five-by-five-network.xml")
    def fiveByFiveUnidirectionalFile: File = new File("matsim/src/main/resources/matsim/network/test-network/five-by-five-unidirectional-network.xml")
    def threeAltPathsFile: File            = new File("matsim/src/main/resources/matsim/network/test-network/three-alt-paths-network.xml")
    def rye: File                          = new File("matsim/src/main/resources/matsim/network/rye-network.xml")
    def louisville: File                   = new File("matsim/src/main/resources/matsim/network/louisville-network.xml")
  }
}

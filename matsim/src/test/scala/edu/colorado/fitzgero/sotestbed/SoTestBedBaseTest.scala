package edu.colorado.fitzgero.sotestbed

import java.io.File

import cats.effect.unsafe.IORuntime

import org.scalatest.{Matchers, WordSpec}

abstract class SoTestBedBaseTest extends WordSpec with Matchers {

  implicit val runtime: IORuntime = cats.effect.unsafe.implicits.global

  object TestNetwork {

    def fiveByFiveNetworkFile: File =
      new File("matsim/src/main/resources/matsim/network/test-network/five-by-five-network.xml")

    def fiveByFiveUnidirectionalFile: File =
      new File("matsim/src/main/resources/matsim/network/test-network/five-by-five-unidirectional-network.xml")

    def threeAltPathsFile: File =
      new File("matsim/src/main/resources/matsim/network/test-network/three-alt-paths-network.xml")
    def rye: File        = new File("matsim/src/main/resources/matsim/network/rye-network.xml")
    def louisville: File = new File("matsim/src/main/resources/matsim/network/louisville-network.xml")
    def loveland: File   = new File("matsim/src/main/resources/matsim/network/loveland-network.xml")
    def boulder: File    = new File("matsim/src/main/resources/matsim/network/boulder-network.xml")
    def denver: File     = new File("matsim/src/main/resources/matsim/network/denver-downtown-network.xml")
  }
}

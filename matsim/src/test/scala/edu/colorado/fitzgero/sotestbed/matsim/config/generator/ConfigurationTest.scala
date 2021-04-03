package edu.colorado.fitzgero.sotestbed.matsim.config.generator

import java.io.{File, PrintWriter}

import scala.util.Random

import kantan.csv._
import kantan.csv.ops._
import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest

class ConfigurationTest extends SoTestBedBaseTest {
  "Generator" when {
    "run" when {
      "called" should {
        "produce reasonable results" in {
          val random = new Random(0)
          val result = Configuration.generate(random, Some("00"))
          val hocons = result.map { _.toHocon }

          val pw = new PrintWriter("/Users/robertfitzgerald/test.conf")
          pw.write(hocons.mkString("\n\n--------------------------------------------------\n\n"))
          pw.close()

          import Configuration._
          val out = new File("/Users/robertfitzgerald/test.csv")
          out.writeCsv(result, rfc.withHeader)

        }
      }
    }
  }
}

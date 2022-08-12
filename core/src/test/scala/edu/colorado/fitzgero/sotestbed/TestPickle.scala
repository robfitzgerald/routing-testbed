package edu.colorado.fitzgero.sotestbed

// import upickle.default.{ReadWriter => RW, macroRW}
import upickle.default._
import java.io._
import scala.util.Try
import scala.util.Failure
import scala.util.Success

case class Thing(i: Int, str: String)

object Thing {
  implicit val rw: ReadWriter[Thing] = macroRW
}

case class Big(i: Int, b: Boolean, str: String, c: Char, t: Thing)

object Big {
  implicit val rw: ReadWriter[Big] = macroRW
}

class TestPickle extends SoTestBedBaseTest {
  "pickles" in {

    val big = Big(1, true, "lol", 'Z', Thing(7, ""))

    // write string
    val pwStr  = new PrintWriter("picked.json")
    val outStr = write(big, indent = 4)
    pwStr.write(outStr)
    pwStr.close()

    // write bytes
    val outBytes = writeBinary(big)
    Try {
      val fos = new FileOutputStream("pickled.txt")
      fos.write(outBytes)
      fos.close()
    } match {
      case Failure(exception) =>
        fail("failed writing bytes", exception)
      case Success(value) =>
        succeed
    }

  }
}

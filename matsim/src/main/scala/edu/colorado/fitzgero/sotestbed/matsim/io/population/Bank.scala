package edu.colorado.fitzgero.sotestbed.matsim.io.population

import java.io.{File, PrintWriter}
import java.nio.file.Path

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma

object Bank {

  val KarmaBankFilename = "karma_bank.csv"

  def readBankLedger(file: File): IO[Map[String, Karma]] = ???

  /**
    * writes the start and end karma values for each agent id to a file in the provided directory location
    * @param initialBank initial bank values
    * @param finalBank final bank values
    * @param directory the directory where to write the ledger
    * @return the output file path string wrapped in the effect of writing this file
    */
  def writeFinalLedger(initialBank: Map[String, Karma], finalBank: Map[String, Karma], directory: Path): IO[String] = {
    val header = "agent,initial,final"
    val rows = for {
      agent        <- initialBank.keySet.union(finalBank.keySet).toList
      agentInitial <- initialBank.get(agent)
      agentFinal   <- finalBank.get(agent)
    } yield f"$agent,$agentInitial,$agentFinal"
    val outString = rows.mkString(header + "\n", "\n", "")
    IO {
      val dest = directory.resolve(KarmaBankFilename).toString
      val pw   = new PrintWriter(dest)
      pw.write(outString)
      pw.close()
      dest
    }
  }
}

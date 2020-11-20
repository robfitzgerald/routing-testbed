package edu.colorado.fitzgero.sotestbed.reports

trait FinalReports[F[_]] {
  def finishReports(): F[Unit]
}

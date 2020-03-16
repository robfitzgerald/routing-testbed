package edu.colorado.fitzgero.sotestbed.reports

trait FinalReports[F[_]] {
  def finishReports(simulator: Any): F[Unit]
}

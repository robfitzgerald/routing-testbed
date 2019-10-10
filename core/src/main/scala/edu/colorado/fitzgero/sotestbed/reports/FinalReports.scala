package edu.colorado.fitzgero.sotestbed.reports

trait FinalReports[F[_]] {
  type Simulator

  def finishReports(simulator: Simulator): F[Unit]
}

package edu.colorado.fitzgero.sotestbed.reports

trait Reports[F[_]] extends RoutingReports with FinalReports[F]

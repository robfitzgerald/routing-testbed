package edu.colorado.fitzgero.sotestbed.reports

abstract class Reports[F[_], V, E] extends RoutingReports[F, V, E] with FinalReports[F]

package edu.colorado.fitzgero.sotestbed.matsim.app

import cats.data.Validated
import cats.implicits._

import com.monovore.decline.Opts
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

object MATSimExperimentCommandLineParsing {


//  lazy private val RunCommandLineOptions: Opts[(Int, SimTime, SimTime, SimTime)] =
//    (iterations, startOfSimTime, endOfSimTime, endOfRoutingTime).tupled

  lazy private val iterations: Opts[Int] = Opts
    .option[Int]("iterations", short = "i", metavar = "i", help = "number of MATSim iterations to run")
    .withDefault(1)

  lazy private val startOfSimTime: Opts[SimTime] = Opts
    .option[Int]("startTime", help = "start of simulation in MATSim time")
    .mapValidated{ t =>
      if (t >= 0) Validated.valid(SimTime(t)) else Validated.invalidNel("must be zero or positive")
    }
    .withDefault(SimTime.StartOfDay)

  lazy private val endOfSimTime: Opts[SimTime] = Opts
    .option[Int]("endTime", help = "end of simulation in MATSim time")
    .mapValidated{ t =>
      if (t > 0) Validated.valid(SimTime(t)) else Validated.invalidNel("must be positive")
    }
    .withDefault(SimTime.EndOfDay)

  lazy val endOfRoutingTime: Opts[SimTime] = Opts
    .option[Int]("endRouteTime", help = "prevent solving routing after this MATSim time")
    .mapValidated{ t =>
      if (t > 0) Validated.valid(SimTime(t)) else Validated.invalidNel("must be positive")
    }
    .withDefault(SimTime.EndOfDay)
}

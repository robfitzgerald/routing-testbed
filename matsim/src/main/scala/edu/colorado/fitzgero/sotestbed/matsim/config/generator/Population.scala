package edu.colorado.fitzgero.sotestbed.matsim.config.generator

object Population {

  def apply(popSize: Int): String = {
    s"""pop.size = $popSize
       |population {
       |  work-activity-min-time = "08:30:00"
       |  work-activity-max-time = "09:30:00"
       |  work-duration-hours    = 8
       |}""".stripMargin
  }
}

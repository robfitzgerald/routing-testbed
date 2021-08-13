package edu.colorado.fitzgero.sotestbed.rllib

sealed trait PolicyClientResponse

object PolicyClientResponse {

  case class StartEpisodeResponse(episode_id: EpisodeId) extends PolicyClientResponse

  /** .. for each [[PolicyClientRequest]] */

  // JSON response payload should be interpretable from the PolicyServerNoPickleInput file
}

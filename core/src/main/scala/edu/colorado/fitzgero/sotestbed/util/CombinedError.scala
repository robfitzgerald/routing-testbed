package edu.colorado.fitzgero.sotestbed.util

case class CombinedError(errors: List[Throwable], optionalTitle: Option[String] = None) extends Error {

  override def getMessage: String = {
    val title = optionalTitle.getOrElse("")
    errors
      .map { e => f"${e.getClass} ${e.getMessage} ${e.getCause}" }
      .mkString(title, "\n", "")
  }
}

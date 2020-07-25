package edu.colorado.fitzgero.sotestbed.util

case class CombinedError(errors: List[Throwable]) extends Error {
  override def getMessage: String =
    errors
      .map { e =>
        f"${e.getClass} ${e.getMessage} ${e.getCause}"
      }
      .mkString("\n")
}

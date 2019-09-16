package edu.colorado.fitzgero.sotestbed.model.agent

import scala.util.matching.Regex

sealed trait RequestClass
object RequestClass {
  case object UE extends RequestClass {
    override def toString: String = "ue"
  }
  case class SO(group: String = "") extends RequestClass {
    override def toString: String = if (group.isEmpty) "so" else s"so-$group"
  }
  val SORegex: Regex = """so-(\w+)""".r.unanchored
  def apply(string: String): Option[RequestClass] = {
    string.toLowerCase match {
      case x: String if x == UE.toString => Some{ UE }
      case SORegex(group) => Some{ SO(group) }
      case _ => None
    }
  }
}
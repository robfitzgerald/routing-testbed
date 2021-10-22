package edu.colorado.fitzgero.sotestbed.rllib

import java.io.File

import cats.effect.IO
import cats.implicits._
import cats.kernel.Monoid

import edu.colorado.fitzgero.sotestbed.util.CirceUtils
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import io.circe.parser._

/**
  * a group is a virtual [[AgentId]] composed of a set of [[AgentId]]s
  * where the reward for a group is the sum of rewards for agents
  *
  * @param agentToGroup a mapping from real agents to grouping agents
  * @param groupToAgent a mapping from each group agent to its agents
  *                     where list order is relevant.
  */
final case class Grouping(
  agentToGroup: Map[AgentId, AgentId],
  groupToAgent: Map[AgentId, List[AgentId]]
) {

  /**
    * group data on its way to the server
    * @param data the data to group
    * @param m monoid providing the "zero" or "empty" value for type T
    * @tparam T the data value type, such as Int, Double, or String
    * @return the grouped data, or, an error
    */
  def group[T](data: Map[AgentId, T])(implicit m: Monoid[T]): Either[Error, Map[AgentId, List[T]]] = {
    val initial: Either[Error, Map[AgentId, Map[AgentId, T]]] =
      Right(groupToAgent.map { case (k, v) => k -> v.map { _ -> m.empty }.toMap })
    val populatedBuilderOrError = data.foldLeft(initial) {
      case (acc, (agentId, t)) =>
        for {
          builder        <- acc
          updatedBuilder <- Grouping.updateValueForAgent(builder, agentToGroup, agentId, t)
        } yield updatedBuilder
    }

    val flattenedBuilderOrError = for {
      builder <- populatedBuilderOrError
    } yield {
      builder.map { case (k, v) => k -> v.values.toList }
    }

    flattenedBuilderOrError
  }

  /**
    * ungroup data that has returned from the server
    * @param data the data to ungroup
    * @tparam T the data value type, such as Int, Double, or String
    * @return the ungrouped data
    */
  def ungroup[T](data: Map[AgentId, List[T]]): Either[Error, Map[AgentId, T]] = {
    val ungrouped = data.toList.traverse {
      case (groupId, values) =>
        groupToAgent
          .get(groupId)
          .toRight(new Error(s"groupId $groupId missing from grouping"))
          .flatMap { agentIds =>
            val notSameLength = agentIds.lengthCompare(values) != 0
            if (notSameLength) {
              Left(new Error(s"group $groupId: received ${values.size} values but have ${agentIds.size} agents"))
            } else {
              Right(agentIds.zip(values))
            }
          }
    }
    ungrouped.map { _.flatten.toMap }
  }
}

object Grouping {

  implicit val obsMapEnc: Encoder[Map[AgentId, List[AgentId]]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val obsMapDec: Decoder[Map[AgentId, List[AgentId]]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (d: List[AgentId]) => Right(d))

  /**
    * reads a grouping from JSON
    * @param file the JSON file with a mapping from GroupId to List of AgentId
    * @return the effect of loading this grouping
    */
  def apply(file: File): IO[Grouping] = {
    val result =
      for {
        source       <- IO.delay { scala.io.Source.fromFile(file) }
        string       <- IO.delay { source.getLines.mkString }
        groupToAgent <- IO.fromEither(decode[Map[AgentId, List[AgentId]]](string))
      } yield {
        // build a reverse mapping, but keep the ordering implied by the groupToAgent
        val agentToGroup =
          groupToAgent
            .foldLeft(Map.empty[AgentId, AgentId]) {
              case (builder, (groupId, agentIds)) =>
                agentIds.foldLeft(builder) { _.updated(_, groupId) }
            }
        Grouping(agentToGroup = agentToGroup, groupToAgent = groupToAgent)
      }

    result
  }

  /**
    *
    *
    * @param builder a mapping from group ids to the agents within their group and their values
    * @param agentToGroup the mapping from agent ids to their group id
    * @param agentId the agent id we are updating
    * @param t the value we are updating
    * @tparam T the type of the value, likely an Int, Double, or String
    * @return the updated builder, or, an error
    */
  def updateValueForAgent[T](
    builder: Map[AgentId, Map[AgentId, T]],
    agentToGroup: Map[AgentId, AgentId],
    agentId: AgentId,
    t: T
  ): Either[Error, Map[AgentId, Map[AgentId, T]]] = {
    for {
      groupId <- agentToGroup.get(agentId).toRight(new Error(s"agentId $agentId missing from grouping"))
      subtree <- builder.get(groupId).toRight(new Error(s"groupId $groupId missing from grouping"))
    } yield {
      val updatedSubtree = subtree.updated(agentId, t)
      builder.updated(groupId, updatedSubtree)
    }
  }
}

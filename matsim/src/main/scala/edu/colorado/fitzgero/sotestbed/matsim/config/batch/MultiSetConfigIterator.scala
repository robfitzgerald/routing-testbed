package edu.colorado.fitzgero.sotestbed.matsim.config.batch

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import scala.collection.JavaConverters._

import com.typesafe.config.Config

/**
  * builds an iterator which creates all combinations for this meta-config
  * @param config the meta-config
  */
case class MultiSetConfigIterator(config: Config) {

  import MultiSetConfigIterator.MultiSetPointer

  val multiSet: Map[String, Array[String]] = {
    for {
      entry <- config.entrySet().asScala.toList
    } yield {
      Try {
        val array: Array[String] = entry.getValue.unwrapped.toString
          .stripPrefix("[")
          .stripSuffix("]")
          .split(",")
        entry.getKey -> array
      } match {
        case Failure(exception) =>
          val msg = s"failure on entry $entry"
          throw new Error(msg, exception)
        case Success(keyValuePair) =>
          keyValuePair
      }
    }
  }.toMap

  /**
    * lists all of the variations on the meta-level config arrays for
    * generating variation-specific configs
    * @return
    */
  def allCombinations: List[List[(String, String)]] = {

    @tailrec
    def recurse(
      pointer: MultiSetPointer,
      solution: List[List[(String, String)]] = List.empty
    ): List[List[(String, String)]] = {

      pointer.getValues(multiSet) match {
        case None => throw new IllegalStateException("nope")
        case Some(partial) =>
          pointer.advance match {
            case None => partial +: solution
            case Some(nextPointer) =>
              recurse(nextPointer, partial +: solution)
          }
      }
    }

    recurse(MultiSetPointer.fromMultiSet(multiSet))
  }
}

object MultiSetConfigIterator {

  /**
    * steps through all multiset combinations
    * @param pos the current position
    * @param max the maximum index value for each set in the multiset (not the size of the set)
    */
  case class MultiSetPointer(
    pos: Array[Int],
    max: Array[Int]
  ) {

    /**
      * advances the pointer through all possible multiset combinations
      * @return the advanced pointer, or None if all indices have been seen
      */
    def advance: Option[MultiSetPointer] = {
      @tailrec
      def _step(idx: Int = pos.length - 1, solution: Array[Int] = pos): Option[Array[Int]] = {
        if (pos(idx) == max(idx)) {
          if (idx == 0) {
            //
            None
          } else {
            _step(idx - 1, solution.updated(idx, 0))
          }
        } else {
          Some {
            solution.updated(idx, solution(idx) + 1)
          }
        }
      }

      _step().map { next => this.copy(pos = next) }
    }

    /**
      * uses the pointer to grab a unique combination of values, one from each subset, of a multiset
      * @param multiSet the multiset to get values from
      * @return the values at the pos of the multiset, or None if something blew up
      */
    def getValues(multiSet: Map[String, Array[String]]): Option[List[(String, String)]] = {
      Try {
        val values: Iterable[(String, String)] = for {
          ((key, values), idx) <- multiSet.zip(pos)
        } yield {
          (key, values(idx))
        }
        values.toList
      }.toOption
    }
  }

  object MultiSetPointer {

    /**
      * builds a MultiSetPointer from a multiset
      * @param multiSet a multiset, where each set is a map from Typesafe Config path to a config value at that path
      * @return a pointer, starting at the zero-index locations
      */
    def fromMultiSet(multiSet: Map[String, Array[String]]): MultiSetPointer = {
      MultiSetPointer(
        pos = Array.fill(multiSet.size)(0),
        max = multiSet.map { case (_, values) => values.length - 1 }.toArray
      )
    }
  }
}

package edu.colorado.fitzgero.sotestbed.util

import scala.annotation.tailrec
import scala.reflect.ClassTag


/**
  * an iterator that will let us perform multi-choice combinations over this multiSet
  * @param multiSet a bag of bags of different sizes and arbitrary element types
  * @param pos the current combination of indices
  * @param max the final combination of indices
  * @tparam V the element type
  */
class MultiSetIterator[V : ClassTag](
  val multiSet: Array[Array[V]],
  var pos: Array[Int],
  val max: Array[Int]) extends Iterator[Array[V]] {

  private var _hasNext: Boolean = !this.pos.sameElements(this.max)

  /**
    * iterator hasNext method
    * @return true if there is another combination to iterate
    */
  override def hasNext: Boolean = _hasNext

  /**
    * returns the next multiset multi-choice combination
    * @return a combination
    */
  override def next(): Array[V] = {
    val result: Array[V] = multiSet.zip(pos).map{ case (arr, idx) => arr(idx) }
    advance()
    result
  }

  /**
    * advances the position of our iterator
    */
  def advance(): Unit = {
    @tailrec
    def _step(idx: Int = pos.length - 1, solution: Array[Int] = pos): Option[Array[Int]] = {
      if (pos(idx) == max(idx)) {
        if (idx == 0) {
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

    _step() match {
      case None =>
        _hasNext = false
      case Some(newPos) =>
        pos = newPos
        _hasNext = true
    }
  }
}

object MultiSetIterator {
  /**
    * initialize a MultiSetIterator from a MultiSet
    * @param multiSet a bag of bags of different sizes and arbitrary element types
    * @tparam V the element type
    * @return an iterator that will let us perform multi-choice combinations over this multiSet
    */
  def apply[V : ClassTag](multiSet: Array[Array[V]]): MultiSetIterator[V] = {
    new MultiSetIterator(
      multiSet = multiSet,
      pos = Array.fill(multiSet.length)(0),
      max = multiSet.map{ _.length - 1 }
    )
  }
}
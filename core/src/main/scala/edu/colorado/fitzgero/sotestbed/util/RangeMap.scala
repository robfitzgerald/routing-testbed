package edu.colorado.fitzgero.sotestbed.util

object RangeMap {

  /**
    * build a RangeMap for the K/V pairs provided.
    *
    * this data structure is based on the stack overflow answer
    * https://stackoverflow.com/a/13400317/4803266
    * which describes using a java.util.TreeMap to make a range-based lookup.
    * this extends that answer by looking for both the upper and lower neighbor
    * and interpolating the result.
    *
    * the RangeMap allows lookup and linear interpolation of values.
    *
    * todo:
    *  - we need to know if the values are ascending or descending as well (add bool field)
    *  - finish implementing linearInterpolate
    *    - find percent of distance between kLower and kUpper after offset to zero
    *    - apply percent value to range between vLower and vUpper offset to zero
    *      - this can be reversed if values are descending
    *
    * @param kvs pairs of key/values to build a lookup from
    * @param sortKeysAscending if false, the domain of the function is inverted
    * @param vRangeLowerBound lower bound in V (wrt the domain) for range of function
    * @param vRangeUpperBound upper bound in V (wrt the domain) for range of function
    * @param kN an instance of scala Numeric for the key type
    * @tparam K the key type
    * @tparam V the value type
    * @return a RangeMap for the provided values
    */
  def apply[K, V](
    kvs: List[(K, V)],
    sortKeysAscending: Boolean,
    vRangeLowerBound: V,
    vRangeUpperBound: V
  )(implicit kN: Numeric[K]): RangeMap[K, V] = {
    // sort data by keys in direction specified by user
    val sortFn: ((K, V)) => Double =
      (tup: (K, V)) => {
        val (k, _) = tup
        if (sortKeysAscending) kN.toDouble(k) else kN.toDouble(kN.negate(k))
      }
    val sorted = kvs.sortBy(sortFn)

    // place all sorted key/value pairs into the treemap
    val tree = new java.util.TreeMap[K, V]
    tree.put(kN.zero, vRangeLowerBound)
    for {
      (k, v) <- sorted
    } {
      tree.put(k, v)
    }

    new RangeMap(tree, sortKeysAscending)
  }

  private[RangeMap] case class QueryNeighbors[K, V](
    lowerK: K,
    upperK: K,
    lowerV: V,
    upperV: V
  )
}

/**
  * see RangeMap.apply method for details
  * @param tree ordered tree data structure
  * @param ascending false if the domain of the function is inverted
  * @tparam K key (domain) type
  * @tparam V value (range) type
  */
class RangeMap[K, V](private val tree: java.util.TreeMap[K, V], val ascending: Boolean) {

  import RangeMap._

  /**
    * attempts to get the value at key without interpolation
    * @param key the key
    * @return the value if key is defined on the tree, otherwise None
    */
  def lookup(key: K): Option[V] = {
    Option(tree.get(key))
  }

  /**
    * interpolates the Value at the provided key from a linear interpolation of the nearest
    * lower and upper entries to key.
    *
    * @param key the key to find an interpolated value in the domain
    * @param kN numeric fractional ops for K (div, minus)
    * @param vN numeric fractional ops for V (div, minus)
    * @return the interpolated value, or None if the query lies outside of the domain of the function
    */
  def linearInterpolate(key: K)(implicit kN: Fractional[K], vN: Fractional[V]): Option[V] = {
    for {
      qn <- findQueryNeighbors(key)
    } yield {
      val interpolated: V = if (ascending) {
        val offsetK      = kN.minus(key, qn.lowerK)
        val offsetU      = kN.minus(qn.upperK, qn.lowerK)
        val offsetVLower = vN.minus(qn.upperV, qn.lowerV)
        val pctInterp    = kN.div(offsetK, offsetU)
        val interpV: V   = ???
        interpV
      } else {
        ???
      }

      interpolated
    }
  }

  /**
    * finds the keys and values that are direct lower and upper neighbors to the provided key
    * @param key the key to lookup as a range lookup
    * @param kN key type
    * @param vN value type
    * @return upper and lower neighbors unless the key falls outside of the function domain
    */
  def findQueryNeighbors(key: K)(implicit kN: Numeric[K], vN: Numeric[V]): Option[QueryNeighbors[K, V]] = {
    for {
      floorEntry <- Option(tree.floorEntry(key))
      ceilEntry  <- Option(tree.ceilingEntry(key))
    } yield QueryNeighbors(
      lowerK = floorEntry.getKey,
      upperK = ceilEntry.getKey,
      lowerV = floorEntry.getValue,
      upperV = ceilEntry.getValue
    )
  }
}

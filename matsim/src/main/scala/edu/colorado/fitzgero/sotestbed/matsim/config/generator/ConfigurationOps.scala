package edu.colorado.fitzgero.sotestbed.matsim.config.generator

import scala.annotation.tailrec

import com.typesafe.scalalogging.LazyLogging

object ConfigurationOps extends LazyLogging {

  /**
    * partitions the collection of configurations, using a striping pattern, into partitions
    * of the same size or size - 1 depending on the parity. first sorts them by problem size
    * (population size * adoption rate). places any with the same name into the same partition.
    *
    * this will roughly load balance
    *
    * @param confs generated configurations
    * @param partitions number of partitions
    * @return configurations partitioned
    */
  def greedyLoadBalancePartitions(
    confs: Seq[Configuration],
    partitions: Int
  ): Iterable[Seq[Configuration]] = {

    val (_, groupedAndSorted) =
      confs
        .groupBy { _.grouping }
        .toList
        .sortBy { case (_, grouped) => grouped.head.sortOrder }
        .unzip

    val solution: Array[List[Configuration]] = Array.fill(partitions)(List.empty[Configuration])

    @tailrec
    def _partition(
      remaining: List[Seq[Configuration]] = groupedAndSorted,
      pos: Int = 0
    ): Array[List[Configuration]] = {
      if (remaining.isEmpty) solution
      else {
        val thisAddition = remaining.head
        val incrPos      = pos + 1
        val nextPos      = if (incrPos % partitions == 0) 0 else incrPos
        val atIndex      = solution(pos)
        solution.update(pos, atIndex ::: thisAddition.toList)
        _partition(remaining.tail, nextPos)
      }
    }

    val result = _partition()

    for {
      (p, pId) <- result.zipWithIndex
    } {
      val avgPopSize = p.map { _.populationSize }.sum.toDouble / p.length
      logger.info(f"partition $pId has avg pop size $avgPopSize%.2f")
    }

    result
  }
}

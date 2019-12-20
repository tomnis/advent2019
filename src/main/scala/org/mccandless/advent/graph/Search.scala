package org.mccandless.advent.graph

import scala.collection.mutable

trait Search[B,S] {


  /**
   * @param board
   * @param cur
   * @return moves we can make from current state
   */
  def neighbors(board: B, cur: S): Seq[S]


  /**
   * finds shortest path length
   *
   * @param board
   * @param start
   * @param end
   * @return
   */
  def shortestPath(board: B, start: S, end: S): Option[Long] = {
    val distancesTo: mutable.Map[S, Long] = mutable.Map(start -> 0L).withDefaultValue(Long.MaxValue)
    var fringe: List[S] = List(start)

    while (fringe.nonEmpty) {
      val cur: S = fringe.head
      fringe = fringe.tail

      neighbors(board, cur) foreach { nebr =>
        val dist: Long = distancesTo(cur) + 1
        if (dist < distancesTo(nebr)) {
          fringe = nebr :: fringe
          distancesTo(nebr) = dist
        }
      }
    }

    distancesTo.get(end)
  }

}

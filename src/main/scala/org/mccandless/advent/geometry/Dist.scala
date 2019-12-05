package org.mccandless.advent.geometry

import scala.math.abs

/**
 *
 * Created by tdm on 2019-12-02.
 */
object Dist {

  def manhattanDistance(p1: Point, p2: Point): Long = abs(p1.x - p2.x) + abs(p1.y - p2.y)
  require(manhattanDistance(Point(0, 0), Point(0, 0)) == 0)
  require(manhattanDistance(Point(0, 0), Point(3, 3)) == 6)
}

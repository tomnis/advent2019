package org.mccandless.advent.geometry

import scala.collection.mutable

import scala.math.{atan2, abs}

/**
 *
 * Created by tdm on 2019-12-02.
 */
case class Point(x: Long, y: Long) {
  import Point._


  def rangeTo(other: Point): Seq[Point] = {
    val dx: Long = other.x - this.x
    val dy: Long = other.y - this.y

    val g: Long = abs(gcd(dx, dy))
    val xStep = dx / g
    val yStep = dy / g

    val points: mutable.Buffer[Point] = mutable.Buffer.empty
    var curPoint: Point = this.copy()

    while (curPoint != other) {
      points += curPoint
      curPoint = curPoint.copy(x = curPoint.x + xStep, y = curPoint.y + yStep)
    }
    points += other
    points.toSeq
//    (this.x to other.x by xStep) zip (this.y to other.y by yStep) map { case (x, y) => Point(x, y) }
  }

  def -(other: Point): Point = Point(this.x - other.x, this.y - other.y)


  def angle(other: Point): Double = {
    val delta: Point = other - this
    atan2(delta.x, delta.y)
  }
}


object Point {

  def gcd(a: Long, b: Long): Long = if (a == 0) b else gcd(b % a, a)
  require(gcd(366, 60) == 6)
  require(gcd(60, 366) == 6)
}

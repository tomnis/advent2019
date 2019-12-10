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
    val delta: Point = other - this
    val g: Long = abs(gcd(delta.x, delta.y))
    val step: Point = Point(delta.x / g, delta.y / g)

    val points: mutable.Buffer[Point] = mutable.Buffer.empty
    var curPoint: Point = this

    while (curPoint != other) {
      points += curPoint
      curPoint += step
    }
    points += other
    points.toSeq
//    (this.x to other.x by xStep) zip (this.y to other.y by yStep) map { case (x, y) => Point(x, y) }
  }

  def -(other: Point): Point = Point(this.x - other.x, this.y - other.y)
  def +(other: Point): Point = Point(this.x + other.x, this.y + other.y)


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

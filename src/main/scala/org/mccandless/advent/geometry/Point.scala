package org.mccandless.advent.geometry

import org.mccandless.advent.ntheory.NumberTheory.gcd
import scala.collection.mutable

import scala.math.{atan2, abs}

/**
 *
 * Created by tdm on 2019-12-02.
 */
case class Point(x: Long, y: Long) {

  override def toString: String = (x, y).toString


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

  def -(other: Point): Point = Point(x - other.x, y - other.y)
  def +(other: Point): Point = Point(x + other.x, y + other.y)




  def plusX(delta: Long): Point = copy(x = x + delta)
  def minusX(delta: Long): Point = plusX(-delta)

  def plusY(delta: Long): Point = copy(y = y + delta)
  def minusY(delta: Long): Point = plusY(-delta)


  def up: Point = minusY(1)
  def down: Point = plusY(1)
  def left: Point = minusX(1)
  def right: Point = plusX(1)


  def angle(other: Point): Double = {
    val delta: Point = other - this
    atan2(delta.x, delta.y)
  }
}

package org.mccandless.advent

import org.mccandless.advent.geometry.Point
import org.mccandless.advent.geometry.Dist.manhattanDistance

import scala.collection.mutable
import scala.math.abs

/**
 *
 * Created by tdm on 2019-12-02.
 */
object Prob3 extends Parser[List[WireSegment]] with App {
  override val inputFileName: String = "prob3_input.txt"

  val origin = Point(0, 0)

  override def parse(line: String): List[WireSegment] = line.split(",").map(WireSegment.apply).toList
  require(parse("U7,R6,D4,L4") == List(Up(7), Right(6), Down(4), Left(4)))

  def increment(oldPoint: Point, segment: WireSegment): Point = segment match {
    case Up(_) => oldPoint.copy(y = oldPoint.y + 1)
    case Down(_) => oldPoint.copy(y = oldPoint.y - 1)
    case Left(_) => oldPoint.copy(x = oldPoint.x - 1)
    case Right(_) => oldPoint.copy(x = oldPoint.x + 1)
  }
  require(increment(origin, Up(1)) == Point(0, 1))
  require(increment(origin, Down(1)) == Point(0, -1))
  require(increment(origin, Left(1)) == Point(-1, 0))
  require(increment(origin, Right(1)) == Point(1, 0))


  def traceWirePaths(wires: Seq[Wire]): Map[Point, Set[Int]] = {
    val grid: mutable.Map[Point, mutable.Set[Int]] = mutable.Map.empty

    // side effect of updating grid, return new point
    def updateGrid(cursor: Point, segment: WireSegment, id: Int): Point = {
      var curPoint = cursor
      for (_ <- 1 to segment.length) {
        curPoint = increment(curPoint, segment)
        if (grid.contains(curPoint)) grid(curPoint) += id
        else grid += (curPoint -> mutable.Set(id))
      }

      curPoint
    }

    for (wire <- wires) {
      println(s"processing wire ${wire.id}")
      var curPoint: Point = Point(0, 0)
      for (segment <- wire.segments) {
        curPoint = updateGrid(curPoint, segment, wire.id)
      }
    }

    grid.view.mapValues(_.toSet).toMap
  }


  // trace wire paths and get intersection points. a wire crossing with itself does not count.
  def solve(wires: List[Wire]): Long = {
    traceWirePaths(wires)
      .filter(_._2.size > 1)
      .keys
      .map(p => manhattanDistance(p, origin))
      .min
  }


  def solve2(wires: List[Wire]): Long = {
    traceWirePaths(wires).filter(_._2.size > 1)
      .keys
      .map(p => combinedStepsToPoint(wires, p))
      .min
  }


  def combinedStepsToPoint(wires: List[Wire], dest: Point): Int = {
    stepsToPoint(wires(0), dest) + stepsToPoint(wires(1), dest)
  }


  def stepsToPoint(wire: Wire, dest: Point): Int = {

    var curPoint = origin
    var steps = 0

    for (segment <- wire.segments) {
      var stepsRemainingInSegment = segment.length
      while (curPoint != dest && stepsRemainingInSegment > 0) {
        curPoint = increment(curPoint, segment)
        steps += 1
        stepsRemainingInSegment -= 1
      }
    }

    steps
  }


  val wires: List[Wire] = input().zipWithIndex.map { case (segments, id) => Wire(segments, id) }.toList

  require(solve(wires) == 403)
  println(solve(wires))
  println(solve2(wires))
  require(solve2(wires) == 4158)
}

case class Wire(segments: List[WireSegment], id: Int)

sealed trait WireSegment {
  val length: Int
}

object WireSegment {
  def apply(s: String): WireSegment = {
    val prefix: Char = s.head
    val length: Int = s.tail.toInt

    prefix match {
      case 'U' => Up(length)
      case 'D' => Down(length)
      case 'L' => Left(length)
      case 'R' => Right(length)
      case unknown => throw new RuntimeException(s"unknown prefix $unknown")
    }
  }
}

case class Up(length: Int) extends WireSegment
case class Down(length: Int) extends WireSegment
case class Left(length: Int) extends WireSegment
case class Right(length: Int) extends WireSegment

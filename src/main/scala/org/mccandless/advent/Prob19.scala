package org.mccandless.advent

import org.mccandless.advent.geometry.Point
import org.mccandless.advent.intcode.Machine
import org.mccandless.advent.intcode.Types.{ParsesIntCode, Program}
import Prob19Types._

import scala.collection.mutable

object Prob19 extends ParsesIntCode with App {
  override val inputFileName = "prob19_input.txt"

  // The program uses two input instructions to request the X and Y position to which the drone should be deployed.
  // Negative numbers are invalid and will confuse the drone; all numbers should be zero or positive.

  def part1(program: Program): Long = {
    val startingState: Machine = Machine(program)
    var pointsInTractor: Long = 0


    (0 to 49) foreach { y =>
      var pointsInRow: Long = 0
      (0 to 49) foreach { x =>
        val curMachine: Machine = startingState.snapshot()
        curMachine.run(Seq(x.toLong, y.toLong))
//        println(s"($x $y  ${curMachine.out}")
        pointsInTractor += curMachine.out
        pointsInRow += curMachine.out
        print(Cell(curMachine.out).rep)
      }
      println(s"  (row $y) has $pointsInRow pulled points")
    }

    pointsInTractor
  }

  println(part1(this.input().next()))




  def scanRowFromScratch(startState: Machine, y: Long): Set[Point] = {
    println(s"scanning row $y from scratch")
    val maxX: Long = 1000
    val res: mutable.Set[Point] = mutable.Set.empty

    (0L to maxX).foreach { x =>
      val p = Point(x,y)
      val m = startState.snapshot()
      val out = m.run(Seq(p.x, p.y)).output

      if (out == 1) {
        res += p
      }
    }

    res.toSet
  }



  def scanLeftAndRight(machine: Machine, start: Point): Set[Point] = {
    val res: mutable.Set[Point] = mutable.Set.empty

    var curPoint: Point = start
    var done: Boolean = false

    // scan right
    while (!done) {
      // check point, then increment
      val curMachine = machine.snapshot()
      curMachine.run(Seq(curPoint.x, curPoint.y))

      Cell(curMachine.out) match {
        case Stationary =>
          done = true
        case Pulled =>
          res += curPoint
          curPoint += Point(1, 0)
      }
    }

    // reset and scan left
    curPoint = start
    done = false

    while (!done && curPoint.x >= 0) {
      // check point, then increment
      val curMachine = machine.snapshot()
      curMachine.run(Seq(curPoint.x, curPoint.y))

      Cell(curMachine.out) match {
        case Stationary =>
          done = true
        case Pulled =>
          res += curPoint
          curPoint += Point(-1, 0)
      }
    }

    res.toSet
  }





  def getAllPulledPointsOnRow(startState: Machine, y: Long, prevRow: Set[Point]): Set[Point] = {
    if (prevRow.isEmpty) scanRowFromScratch(startState, y)
    else {
      // start from leftmost point, scan left and right
      val leftMost: Point = prevRow.minBy(_.x)
      // start from rightmost point, scan left and right
      val rightMost: Point = prevRow.maxBy(_.x)

      // some border conditions
      val res = scanLeftAndRight(startState, Point(leftMost.x, y)) ++ scanLeftAndRight(startState, Point(rightMost.x, y))
      if (res.isEmpty) scanRowFromScratch(startState, y)
      else res
    }
  }


  def checkXFits(row: Set[Point], candidateTopLeft: Point, width: Long): Boolean = {
    row.size >= width && row.count(_.x >= candidateTopLeft.x) >= width
  }



  def checkYFits(startState: Machine, candidateTopLeft: Point, height: Long): Boolean = {
    Seq(candidateTopLeft.y, (candidateTopLeft.y + height - 1)).forall { newY =>
      val newPoint: Point = candidateTopLeft.copy(y = newY)
      val curMachine = startState.snapshot()
      curMachine.run(Seq(newPoint.x, newPoint.y)).output == 1L
    }
  }


  // Find the 100x100 square closest to the emitter that fits entirely within the tractor beam
  // within that square, find the point closest to the emitter
  // What value do you get if you take that point's X coordinate, multiply it by 10000, then add the point's Y coordinate?
  def part2(program: Program): Long = {
    val width: Long = 100
    val height: Long = width
    val startingState: Machine = Machine(program)


    var prevRow: Set[Point] = Set.empty

    // pretty suboptimal
    (0 to 10000).foreach { i =>

      // get all the pulled points on this row
      val pulledPointsInRow: Set[Point] = getAllPulledPointsOnRow(startingState, i, prevRow)
      println(s"${pulledPointsInRow.size} pulled points in row $i")

      pulledPointsInRow.foreach { candidateTopLeft =>
        if (checkXFits(pulledPointsInRow, candidateTopLeft, width) && checkYFits(startingState, candidateTopLeft, height)) {
          println(s"found point $candidateTopLeft")
          return candidateTopLeft.x * 10000 + candidateTopLeft.y
        }
      }

      prevRow = pulledPointsInRow
    }

    // 2500200 too low
    -1
  }

  println(part2(this.input().next()))
}


object Prob19Types {

  type Grid = Set[Point]

  sealed trait Cell {
    val rep: Char
  }
  case object Stationary extends Cell {
    override val rep: Char = '.'
  }
  case object Pulled extends Cell {
    override val rep: Char = '#'
  }
  object Cell {
    def apply(c: Long): Cell = c match {
      case 0 => Stationary
      case 1 => Pulled
      case _ => ???
    }
  }
}

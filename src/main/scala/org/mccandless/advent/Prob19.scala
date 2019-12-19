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

    (0 to 49) foreach { x =>
      (0 to 49) foreach { y =>
        val curMachine: Machine = startingState.snapshot()
        curMachine.run(Seq(x.toLong, y.toLong))
//        println(s"($x $y  ${curMachine.out}")
        pointsInTractor += curMachine.out
      }
    }

    pointsInTractor
  }

  println(part1(this.input().next()))




  def xAxisFits(grid: Grid, p: Point, width: Long): Boolean = {
    (p.x until p.x + width).forall(curX => grid.contains(p.copy(x = curX)))
  }


  def yAxisFits(grid: Grid, p: Point, height: Long): Boolean = {
    (p.y until p.y + height).forall(curY => grid.contains(p.copy(y = curY)))
  }


  def pointFits(grid: Grid, p: Point, width: Long, height: Long): Boolean = {
    xAxisFits(grid, p, width) && yAxisFits(grid, p, height)
  }



  def getAllPulledPointsOnRow(program: Program, c: Long): Set[Point] = {
    val res: mutable.Set[Point] = mutable.Set(Point(c, c))

    var cell: Cell = Pulled
    var curX: Long = c
    // scan to the right
    while (cell == Pulled) {
      curX = curX + 1
      val curMachine = Machine(program)
      curMachine.run(Seq(curX, c))

      cell = Cell(curMachine.out)

      if (cell == Pulled) {
        res += Point(curX, c)
      }
    }

    // reset and scan to the left
    cell = Pulled
    curX = c
    while (cell == Pulled) {
      curX = curX - 1
      val curMachine = Machine(program)
      curMachine.run(Seq(curX, c))

      cell = Cell(curMachine.out)

      if (cell == Pulled) {
        res += Point(curX, c)
      }
    }


    res.toSet
  }



  def checkXFits(row: Set[Point], candidateTopLeft: Point, width: Long): Boolean = {
    row.size >= width && row.count(_.x >= candidateTopLeft.x) >= width
  }


  // Find the 100x100 square closest to the emitter that fits entirely within the tractor beam
  // within that square, find the point closest to the emitter
  // What value do you get if you take that point's X coordinate, multiply it by 10000, then add the point's Y coordinate?
  def part2(program: Program): Long = {
    val width: Long = 10
    val height: Long = width
    val startingState: Machine = Machine(program)


//    val beam: mutable.Set[Point] = mutable.Set.empty


    // startscanning at x,y
//    (width to 1000).foreach { x =>
//      println(s"checking $x")
//      (height to 1000).foreach { y =>
//        val curMachine: Machine = startingState.snapshot()
//        val p: Point = Point(x, y)
//        curMachine.run(Seq(p.x, p.y))
//
//        if (curMachine.out == 1) {
//          beam += (p)
//        }
//
//
//        if (pointFits(beam.toSet, p - Point(10, 10), width, height)) {
//          return p.x * 10000 + p.y
//        }
//      }
//    }

    (9 to 100).foreach { i =>

      // get all the pulled points on this row
      val pulledPointsInRow: Set[Point] = getAllPulledPointsOnRow(program, i)
      println(s"${pulledPointsInRow.size} pulled points in row $i")
      // starting from the left, scan from each point rightward

    }


    // 2500200 too low
    0
  }


  println(part2(this.input().next()))
}


object Prob19Types {

  type Grid = Set[Point]

  sealed trait Cell
  case object Stationary extends Cell
  case object Pulled extends Cell
  object Cell {
    def apply(c: Long): Cell = c match {
      case 0 => Stationary
      case 1 => Pulled
      case _ => ???
    }
  }
}

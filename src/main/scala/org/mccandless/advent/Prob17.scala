package org.mccandless.advent

import org.mccandless.advent.Prob17Types._
import org.mccandless.advent.geometry.Point
import org.mccandless.advent.intcode.Machine
import org.mccandless.advent.intcode.Types.{ParsesIntCode, Program}

import scala.collection.mutable

/**
 * Created by tomas.mccandless on 12/16/19.
 */
object Prob17 extends ParsesIntCode with App {
  override val inputFileName = "prob17_input.txt"

  def alignmentParameter(p: Point): Long = p.x * p.y

  def isIntersectionPoint(map: Map[Point, Char], p: Point): Boolean = {
    val withDef = map.withDefaultValue('.')

    val above = p.copy(y = p.y - 1)
    val below = p.copy(y = p.y + 1)
    val left = p.copy(x = p.x - 1)
    val right = p.copy(x = p.x + 1)

    Seq(above, below, left, right).forall(withDef(_) == '#')
  }

  def intersectionPoints(map: Map[Point, Char]): Seq[Point] = map.keys.filter(isIntersectionPoint(map, _)).toSeq

  def renderMap(map: Map[Point, Char]): Unit = {
    val xMin = map.keys.minBy(_.x).x
    val xMax = map.keys.maxBy(_.x).x
    val yMin = map.keys.minBy(_.y).y
    val yMax = map.keys.maxBy(_.y).y

    for (y <- yMin to yMax) {
      for (x <- xMin to xMax) {
        print(map(Point(x,y)))
      }
      println()
    }
  }

//  def getMapSize(map: Map[Point, Char]): Long = {
//    val xMin = map.keys.minBy(_.x).x
//    val xMax = map.keys.maxBy(_.x).x
//    val yMin = map.keys.minBy(_.y).y
//    val yMax = map.keys.maxBy(_.y).y
//
//    (xMax - xMin + 1) * (yMax - yMin + 1)
//  }

  def getMap(program: Program): Map[Point, Char] = {
    val m: Machine = Machine(program)

    val map: mutable.Map[Point, Char] = mutable.Map.empty
    var x: Long = 0
    var y: Long = 0
    while(!m.halted) {
      val o = m.run().output

      if (o == 10) {
        y += 1
        x = 0
      }
      else {
        map += (Point(x, y) -> o.toChar)
        x += 1
      }
    }
    map.toMap
  }

  def part1(program: Program): Long = {
    // get map
    val map: Map[Point, Char] = getMap(program)
    renderMap(map)
    // get intersection points alingment parameters
    // What is the sum of the alignment parameters
    intersectionPoints(map).map(alignmentParameter).sum
  }

  println(part1(this.input().next()))


  // part 2
  // visit every part of the scaffold at least once.
  // Force the vacuum robot to wake up by changing the value in your ASCII program at address 0 from 1 to 2
  // will use input instructions to receive them, but they need to be provided as ASCII code
  // end each line of logic with a single newline, ASCII code 10
  // First, you will be prompted for the main movement routine.
  // The main routine may only call the movement functions: A, B, or C
  // A,A,B,C,B,C,B,C10
  // Then, you will be prompted for each movement function.
  // Movement functions may use L to turn left, R to turn right, or a number to move forward that many units
  // 10,L,8,R,610
  // Finally, you will be asked whether you want to see a continuous video feed; provide either y or n and a newline
  // once it finishes the programmed set of movements,
  // assuming it hasn't drifted off into space,
  // the cleaning robot will return to its docking station and report the amount of space dust it collected as a large, non-ASCII value in a single output instruction.
  // After visiting every part of the scaffold at least once, how much dust does the vacuum robot report it has collected?

  def getPath(map: Map[Point, Char]): Seq[MovementCommand] = {

    Seq(TurnLeft, MoveForward(5))
  }

  def compressPath(path: Seq[MovementCommand]): MovementStrategy = {
    MovementStrategy(Seq(A, A), path, Seq.empty, Seq.empty)
  }


  def part2(program: Program): Long = {
    require(program.head == 1L)
    // get map
    val map: Map[Point, Char] = getMap(program)
    // force robot to wake up
    val awakeProgram = 2L +: program.tail
    val vacuum = Machine(awakeProgram)

    // plan and compress our path
    val path: Seq[MovementCommand] = getPath(map)
    val strat: MovementStrategy = compressPath(path)

    // prompted for main movement function, then each subroutine
    val main: Seq[Long] = encode(strat.main.map(_.code))
    val subA: Seq[Long] = encode(strat.fA.map(_.code))
    val subB: Seq[Long] = encode(strat.fB.map(_.code))
    val subC: Seq[Long] = encode(strat.fC.map(_.code))

    vacuum.run(main)
    vacuum.run(subA)
    vacuum.run(subB)
    vacuum.run(subC)

    // continuous video feed?
    // requires a significant amount of processing power, and may even cause your Intcode computer to overheat
    var output = vacuum.run(Seq('y'.toLong, newLine))

    while(!vacuum.halted) {
      print(output.output.toChar)
      if (output.output == tumbling) {
        throw new RuntimeException(s"vacuum bot is tumbling in space")
      }
      output = vacuum.run()
    }

    vacuum.out
  }

  part2(input().next)
}

object Prob17Types {
  val comma: Long = 44
  val newLine: Long = 10
  val scaffold: Long = 35
  val space: Long = 46
  val tumbling: Long = 'X'.toLong

  // intersperse commas and append newline
  def encode(code: Seq[Long]): Seq[Long] = code.flatMap(c => Seq(c, comma)).dropRight(1) :+ newLine

  trait Code {
    val rep: Char
    lazy val code: Long = this.rep.toLong
  }

  sealed trait Routine extends Code
  case object A extends Routine {
    override val rep: Char = 'A'
  }
  case object B extends Routine {
    override val rep: Char = 'B'
  }
  case object C extends Routine {
    override val rep: Char = 'C'
  }


  sealed trait MovementCommand extends Code
  case object TurnLeft extends MovementCommand {
    override val rep: Char = 'L'
  }
  case object TurnRight extends MovementCommand {
    override val rep: Char = 'R'
  }
  case class MoveForward(steps: Long) extends MovementCommand {
    override val rep: Char = '_'
    override lazy val code: Long = steps
  }

  // sequence of A, B, or C
  case class MovementStrategy(main: Seq[Routine], fA: Seq[MovementCommand], fB: Seq[MovementCommand], fC: Seq[MovementCommand])
}

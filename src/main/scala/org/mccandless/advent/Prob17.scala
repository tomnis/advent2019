package org.mccandless.advent

import org.mccandless.advent.Prob11Types.{Left90, Right90, Rotate}
import org.mccandless.advent.Prob17Types._
import org.mccandless.advent.geometry.Point
import org.mccandless.advent.intcode.Machine
import org.mccandless.advent.intcode.Types.{ParsesIntCode, Program}
import org.mccandless.advent.util.cardinal.{Cardinal, East, North, South, West}

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
    while(!m.isHalted) {
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



  def getStartingState(value: Map[Point, Char]) = ???

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



    // get starting state, scan map for our location and orientation
    val start: State = getStartingState(map)

    val distancesTo: mutable.Map[State, Long] = mutable.Map.empty.withDefaultValue(Long.MaxValue)
    distancesTo += (start -> 0L)
    var states: List[State] = List(start)

    while (states.nonEmpty) {
      val curState = states.head
      states = states.tail


      // get validMoves
//      val paths = getValidMoves(map, curState)

    }




    Seq(TurnLeft, MoveForward(10), TurnRight, MoveForward(8), TurnLeft, MoveForward(6), TurnRight, MoveForward(6), TurnLeft, MoveForward(8))
  }

  def compressPath(path: Seq[MovementCommand]): MovementStrategy = {
    MovementStrategy(Seq(A), path, path, path)
  }




  def runUntilInput(m: Machine): Unit = while(!m.isAwaitingInput) print(m.run().output.toChar)



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
    val main: Seq[Long] = encode(strat.main)
    if (main.length > 21) throw new RuntimeException("main is too long")

    val subA: Seq[Long] = encode(strat.fA)
    if (subA.length > 21) throw new RuntimeException("subA is too long")

    val subB: Seq[Long] = encode(strat.fB)
    if (subB.length > 21) throw new RuntimeException("subB is too long")

    val subC: Seq[Long] = encode(strat.fC)
    if (subC.length > 21) throw new RuntimeException("subC is too long")

    runUntilInput(vacuum)
    print(vacuum.run(main).output.toChar)
    runUntilInput(vacuum)
    print(vacuum.run(subA).output.toChar)
    runUntilInput(vacuum)
    print(vacuum.run(subB).output.toChar)
    runUntilInput(vacuum)
    print(vacuum.run(subC).output.toChar)
    runUntilInput(vacuum)

    // continuous video feed?
    // requires a significant amount of processing power, and may even cause your Intcode computer to overheat
    print(vacuum.run(Seq('y'.toLong, newLine)).output.toChar)

    while(!vacuum.isHalted) {
      vacuum.run()
      print(vacuum.out.toChar)
      if (vacuum.out == tumbling) {
        throw new RuntimeException(s"vacuum bot is tumbling in space")
      }
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


  def encode(commands: Seq[Code]): Seq[Long] = commands.map(_.rep).mkString(",").map(_.toLong) :+ newLine

  trait Code {
    val rep: String
  }

  sealed trait Routine extends Code
  case object A extends Routine {
    override val rep: String = "A"
  }
  case object B extends Routine {
    override val rep: String = "B"
  }
  case object C extends Routine {
    override val rep: String = "C"
  }


  sealed trait MovementCommand extends Code
  case object TurnLeft extends MovementCommand {
    override val rep: String = "L"
  }
  case object TurnRight extends MovementCommand {
    override val rep: String = "R"
  }
  case class MoveForward(steps: Long) extends MovementCommand {
    override val rep: String = steps.toString
  }

  // sequence of A, B, or C
  case class MovementStrategy(main: Seq[Routine], fA: Seq[MovementCommand], fB: Seq[MovementCommand], fC: Seq[MovementCommand])


  // store (direction, point)
  case class State(direction: Cardinal, point: Point, visited: Set[Point]) {



    def next(cmd: MovementCommand): State = (direction, point, cmd) match {
      case (North, _, TurnLeft) => this.copy(direction = West)
      case (North, _, TurnRight) => this.copy(direction = East)
      case (North, p, MoveForward(steps)) => this.copy(point = p + Point(0, -steps))

      case (South, _, TurnLeft) => this.copy(direction = East)
      case (South, _, TurnRight) => this.copy(direction = West)
      case (South, p, MoveForward(steps)) => this.copy(point = p + Point(0, steps))

      case (East, _, TurnLeft) => this.copy(direction = North)
      case (East, _, TurnRight) => this.copy(direction = South)
      case (East, p, MoveForward(steps)) => this.copy(point = p + Point(steps, 0))

      case (West, _, TurnLeft) => this.copy(direction = South)
      case (West, _, TurnRight) => this.copy(direction = North)
      case (West, p, MoveForward(steps)) => this.copy(point = p + Point(-steps, 0))
    }
  }









}

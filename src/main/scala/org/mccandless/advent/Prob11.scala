package org.mccandless.advent

import org.mccandless.advent.Prob11Types._
import org.mccandless.advent.geometry.Point
import org.mccandless.advent.intcode.Machine
import org.mccandless.advent.intcode.Types.Program

import scala.collection.mutable

object Prob11 extends Parser[Program] with App {
  override val inputFileName = "prob11_input.txt"

  override def parse(line: String): Program = line.split(",").toSeq.map(_.toLong)



  // hull painting robot
  // all panels currently black

  // program uses input instructiosn for camera: 0 if black, 1 if white


  // output a value indicating direction for robot to turn 0 left, 1 riht


  // after turning, move forward 1 panel


  def getNextPosition(point: Point, direction: Direction): Point = direction match {
    case Up => point.copy(y = point.y - 1)
    case Down => point.copy(y = point.y + 1)
    case Left => point.copy(x = point.x - 1)
    case Right => point.copy(x = point.x + 1)
  }
  // starts facing up


  def getNextDirection(dir: Direction, turn: Turn): Direction = {
    (dir, turn) match {
      case (Up, TurnLeft) => Left
      case (Up, TurnRight) => Right
      case (Down, TurnLeft) => Right
      case (Down, TurnRight) => Left
      case (Left, TurnLeft) => Down
      case (Left, TurnRight) => Up
      case (Right, TurnLeft) => Up
      case (Right, TurnRight) => Down
    }
  }


  // how many panels are painted at least once?

  def part1(program: Program): Hull = {
    val hull: mutable.Map[Point, PanelColor] = mutable.Map.empty.withDefaultValue(Black) ++ Map(Point(0, 0) -> Black)
    val m: Machine = Machine(program.toArray)

    var facingDirection: Direction = Up

    var curPosition: Point = Point(0,0)


    var halted = false

    var count: Int = 0

    while (!halted) {


      // run with input of current color
      val currentColor: PanelColor = hull(curPosition)

      // output will be a value to paint the panel
      val newColor: Long = m.run(Seq(currentColor.rep)).output

      // next output will be a value to turn
      val turnEndState = m.run()
      val turn = turnEndState.output

      println(s"new color: ${PanelColor(newColor)},   turn: ${Turn(turn)}")


      // paint the color
      if (hull(curPosition) != PanelColor(newColor)) {
        count += 1
      }
      hull += (curPosition -> PanelColor(newColor))

      // turn
      facingDirection = getNextDirection(facingDirection, Turn(turn))

      // update position
      curPosition = getNextPosition(curPosition, facingDirection)

      //
      halted = turnEndState.halted
    }


    hull.toMap
  }



  val soln1: Hull = part1(this.input().next() ++ Array.fill(10000)(0L))
  println(soln1.size)




  def part2(hull: Hull): Unit = {

    val withDefault = hull.withDefaultValue(Black)

    val minx = withDefault.keys.minBy(_.x).x
    val maxx = withDefault.keys.maxBy(_.x).x
    val miny = withDefault.keys.minBy(_.y).y
    val maxy = withDefault.keys.maxBy(_.y).y


    miny.to(maxy).foreach { y =>

      minx.to(maxx).foreach { x =>

        val pixel = withDefault(Point(x, y)) match {
          case Black => " "
          case White => "+"
        }
        print(pixel)
      }

      println()
    }


  }


  part2(soln1)
}





object Prob11Types {

  type Hull = Map[Point, PanelColor]

  val BLACK = 0
  val WHITE = 1

  sealed trait PanelColor {
    val rep: Long
  }
  case object White extends PanelColor {
    override val rep: Long = 1
  }
  case object Black extends PanelColor {
    override val rep: Long = 0
  }

  object PanelColor {
    def apply(c: Long): PanelColor = c match {
      case 0 => Black
      case 1 => White
      case _ => ???
    }

  }





  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction

  sealed trait Turn
  case object TurnLeft extends Turn
  case object TurnRight extends Turn

  object Turn {
    def apply(t: Long): Turn = t match {
      case 0 => TurnLeft
      case 1 => TurnRight
      case _ => ???
    }
  }
}

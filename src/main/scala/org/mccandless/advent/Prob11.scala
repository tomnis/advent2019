package org.mccandless.advent

import org.mccandless.advent.Prob11Types._
import org.mccandless.advent.geometry.Point
import org.mccandless.advent.intcode.{Halted, Machine, Paused}
import org.mccandless.advent.intcode.Types.{Program, ParsesIntCode}
import org.mccandless.advent.util.cardinal._

import scala.collection.mutable

object Prob11 extends App with ParsesIntCode {
  override val inputFileName = "prob11_input.txt"

  // hull painting robot
  // all panels currently black
  // program uses input instructiosn for camera: 0 if black, 1 if white
  // output a value indicating direction for robot to turn 0 left, 1 riht
  // after turning, move forward 1 panel
  // starts facing up

  def getNextPosition(point: Point, dir: Cardinal): Point = dir match {
    case North => point.copy(y = point.y - 1)
    case South => point.copy(y = point.y + 1)
    case West => point.copy(x = point.x - 1)
    case East => point.copy(x = point.x + 1)
  }


  def getNextDirection(dir: Cardinal, rot: Rotate): Cardinal = (dir, rot) match {
    case (North, Left90) => West
    case (North, Right90) => East
    case (South, Left90) => East
    case (South, Right90) => West
    case (West, Left90) => South
    case (West, Right90) => North
    case (East, Left90) => North
    case (East, Right90) => South
  }


  // how many panels are painted at least once?

  def part1(program: Program): Hull = {
    val m: Machine = Machine(program)

    val hull: mutable.Map[Point, PanelColor] = mutable.Map.empty.withDefaultValue(Black) ++ Map(Point(0, 0) -> Black)
    var facingDirection: Cardinal = North
    var curPosition: Point = Point(0,0)
    var halted = false

    while (!halted) {
      // run with input of current color
      val currentColor: PanelColor = hull(curPosition)
      // output will be a value to paint the panel
      val newColor: Long = m.run(Seq(currentColor.rep)).output
      // next output will be a value to turn
      val turnEndState = m.run()

      turnEndState match {
        case Paused(output) =>
          // paint the color
          hull += (curPosition -> PanelColor(newColor))
          // turn
          facingDirection = getNextDirection(facingDirection, Rotate(output))
          // update position
          curPosition = getNextPosition(curPosition, facingDirection)
        case Halted(_) =>
          halted = true
      }
    }

    hull.toMap
  }



  val soln1: Hull = part1(this.input().next())
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


  sealed trait Rotate
  case object Left90 extends Rotate
  case object Right90 extends Rotate

  object Rotate {
    def apply(t: Long): Rotate = t match {
      case 0 => Left90
      case 1 => Right90
      case _ => ???
    }
  }
}

package org.mccandless.advent

import org.mccandless.advent.Prob13Types.{Ball, Block, Empty, JoystickTilt, Left, Neutral, Paddle, Right, Tile}
import org.mccandless.advent.geometry.Point
import org.mccandless.advent.intcode.{AwaitingInput, Halted, Machine, MachineState, Paused}
import org.mccandless.advent.intcode.Types.{ParsesIntCode, Program}

import scala.collection.mutable

/**
 * Created by tomas.mccandless on 12/12/19.
 */
object Prob13 extends ParsesIntCode with App {
  override val inputFileName = "prob13_input.txt"

  // draw tiles to the screen with output instructions:
  // every three output instructions specify
  // x position (distance from the left),
  // y position (distance from the top),
  // and tile id

  def render(screen: Map[Point, Tile], score: Long = 0): Unit = {
    val withDefault = screen.withDefaultValue(Empty)

    val minx = withDefault.keys.minBy(_.x).x
    val maxx = withDefault.keys.maxBy(_.x).x
    val miny = withDefault.keys.minBy(_.y).y
    val maxy = withDefault.keys.maxBy(_.y).y

    miny.to(maxy).foreach { y =>
      minx.to(maxx).foreach { x =>
        print(withDefault(Point(x,y)).rep)
      }
      println()
    }
    println(s"score:> $score")
  }

  // start the game. how many block tiles?
  def part1(program: Program): Long = {
    val m = Machine(program)
    val screen: mutable.Map[Point, Tile] = mutable.Map.empty

    var halted = false

    while(!halted) {
      val x = m.run().output
      val y = m.run().output
      val tileState = m.run()
      val tile = tileState.output

      screen += (Point(x, y) -> Tile(tile))
      halted = tileState.isInstanceOf[Halted]
    }

//    render(screen.toMap)
    screen.count(_._2 == Block)
  }

//  println(part1(this.input().next()))

  def getBall(screen: Map[Point, Tile]): Point = {
    val x = screen.filter(_._2 == Ball)
    require(x.size == 1)
    screen.filter(_._2 == Ball).head._1
  }

  def getPaddle(screen: Map[Point, Tile]): Point = {
    val x = screen.filter(_._2 == Paddle)
    require(x.size == 1)
    screen.filter(_._2 == Paddle).head._1
  }

  def getInput(ballX: Point, paddleX: Point): JoystickTilt = {
    if (ballX.x < paddleX.x) Left
    else if (ballX.x > paddleX.x) Right
    else Neutral
  }


  // part 2
  // memory address 0 represents the number of quarters that have been inserted; set it to 2 to play for free
  // When three output instructions specify X=-1, Y=0, the third output instruction is not a tile, but is score
  def part2(program: Program): Long = {
    val m = Machine(2 :: program.tail.toList)
    var input: Option[Long] = None
    var gameDone = false
    var score = 0L
    val screen: mutable.Map[Point, Tile] = mutable.Map.empty
    while(!gameDone) {

      val (screenDiff, s) = draw(m, input)
      if (!m.halted) {
        screen ++= screenDiff
        score = s
        val ballPos = this.getBall(screen.toMap)
        val paddlePos = this.getPaddle(screen.toMap)
        val tilt = this.getInput(ballPos, paddlePos)
              render(screen.toMap,score)
        Thread.sleep(1000)
        println(s"ballPos: $ballPos")
        println(s"paddlePos: $paddlePos")
        println(s"joystick: $tilt")
        input = Option(tilt.rep)
      }

      gameDone = m.halted
    }


    score
  }



  def draw(m: Machine, input: Option[Long] = None): (Map[Point, Tile], Long) = {

    val screen: mutable.Map[Point, Tile] = mutable.Map.empty
    var score = 0L
    var doneDrawing = false
    while (!doneDrawing && !m.halted) {
      val x = m.run(input.toSeq).output
      val y = m.run().output
      val tileState = m.run()
      if (x == -1 && y == 0) {
        score = tileState.output
        println(s"draw: set score $score. should be done drawing?")
        doneDrawing = true // ??
      }
      else {
        val tile = tileState.output
        screen += (Point(x, y) -> Tile(tile))
      }
    }

    (screen.toMap, score)
  }

  println(part2(this.input().next()))
}

object Prob13Types {


//  0 is an empty tile. No game object appears in this tile.
//  1 is a wall tile. Walls are indestructible barriers.
//  2 is a block tile. Blocks can be broken by the ball.
//  3 is a horizontal paddle tile. The paddle is indestructible.
//  4 is a ball tile. The ball moves diagonally and bounces off objects.

  sealed trait Tile {
    val rep: Char
  }

  case object Empty extends Tile {
    override val rep: Char = ' '
  }
  case object Wall extends Tile {
    override val rep: Char = 'W'
  }
  case object Block extends Tile {
    override val rep: Char = 'B'
  }
  case object Paddle extends Tile {
    override val rep: Char = '_'
  }
  case object Ball extends Tile {
    override val rep: Char = 'o'
  }

  object Tile {
    def apply(id: Long): Tile = id match {
      case 0 => Empty
      case 1 => Wall
      case 2 => Block
      case 3 => Paddle
      case 4 => Ball
      case _ => ???
    }
  }



  sealed trait JoystickTilt {
    val rep: Long
  }
  case object Neutral extends JoystickTilt {
    override val rep: Long = 0
  }
  case object Left extends JoystickTilt {
    override val rep: Long = -1
  }
  case object Right extends JoystickTilt {
    override val rep: Long = 1
  }

  object JoystickTilt {
    def apply(id: Long): JoystickTilt = id match {
      case 0 => Neutral
      case -1 => Left
      case 1 => Right
      case _ => ???
    }
  }
}


//var doneDrawing = false
//
//while(!doneDrawing) {
//println(s"running with input: $input")
//val x = m.run().output
//val y = m.run().output
//val tileState = m.run()
//if (x == -1 && y == 0) {
//score = tileState.output
//println(s"set score $score. should be done drawing?")
//doneDrawing = true
//}
//else {
//val tile = tileState.output
//screen += (Point(x, y) -> Tile(tile))
//}
//
//tileState match {
//// we are expecting another output
//case Paused(_) =>
//println("paused while drawing, more outputs coming")
//case AwaitingInput(_) =>
//println("awaiting input")
//doneDrawing = true
//case Halted(_) =>
//println("halted")
//doneDrawing = true
//gameDone = true
//}
//}
//
//
//if (!gameDone) {
//val ballPos = this.getBall(screen.toMap)
//val paddlePos = this.getPaddle(screen.toMap)
//val tilt = this.getInput(ballPos.x, paddlePos.x)
//println(s"joystick: $tilt")
//input = Option(tilt.rep)
//}
//render(screen.toMap, score)
//Thread.sleep(100)
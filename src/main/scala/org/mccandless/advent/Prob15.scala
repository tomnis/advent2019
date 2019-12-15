package org.mccandless.advent

import org.mccandless.advent.Prob15Types._
import org.mccandless.advent.geometry.Point
import org.mccandless.advent.intcode.Machine
import org.mccandless.advent.intcode.Types.{ParsesIntCode, Program}

import scala.collection.mutable

/**
 * Created by tomas.mccandless on 12/14/19.
 */
object Prob15 extends ParsesIntCode with App {

  override val inputFileName: String = "prob15_input.txt"

  // Accept a movement command via an input instruction.
  // Send the movement command to the repair droid.
  // Wait for the repair droid to finish the movement operation.
  // Report on the status of the repair droid via an output instruction.


  def repairDroid(program: Program): Long = {
    val m = Machine(program)
    var status: RepairDroidStatus = HitWall

    val visited: mutable.Set[Point] = mutable.Set.empty
    visited += Point(0,0)

    val x = m.run(MoveNorth.rep)
    println(RepairDroidStatus(x.output))

    val mprime = m.snapshot()
    require(mprime.ip == m.ip)


    var states: List[(Point, Machine)] = List((Point(0,0),m))
    while(states.nonEmpty) {
      println(s"checking ${states.size} states")
      val (curPoint, curState) = states.head
      states = states.tail

      // check north
      val goingNorth = curState.snapshot()
      val wentNorth = goingNorth.run(MoveNorth.rep)
      // cartesian coords
      val northPoint = curPoint + Point(0, 1)
      RepairDroidStatus(wentNorth.output) match {
        case HitWall => println("hit wall")
        case Moved if !states.map(_._1).contains(northPoint) =>
          states = states :+ (northPoint, goingNorth)
        case Moved => println(s"already visited")
        case FoundOxygen =>
          println(s"found it! $northPoint")
          states = Nil
          return 0
      }

      // check south
      val goingSouth = curState.snapshot()
      val wentSouth = goingSouth.run(MoveSouth.rep)
      val southPoint = curPoint + Point(0, -1)
      RepairDroidStatus(wentSouth.output) match {
        case HitWall => println("hit wall")
        case Moved if !states.map(_._1).contains(southPoint) =>
          states = states :+ (southPoint, goingSouth)
        case Moved => println(s"already visited")
        case FoundOxygen =>
          println(s"found it! $southPoint")
          states = Nil
          return 0
      }

      // check west
      val goingWest = curState.snapshot()
      val wentWest = goingWest.run(MoveWest.rep)
      val westPoint = curPoint + Point(-1, 0)
      RepairDroidStatus(wentWest.output) match {
        case HitWall => println("ht wall")
        case Moved if !states.map(_._1).contains(westPoint) =>
          states = states :+ (westPoint, goingWest)
        case Moved => println(s"already visited")
        case FoundOxygen =>
          println(s"found it! $westPoint")
          states = Nil
          return 0
      }

      // check east
      val goingEast = curState.snapshot()
      val wentEast = goingEast.run(MoveEast.rep)
      val eastPoint = curPoint + Point(1, 0)
      RepairDroidStatus(wentEast.output) match {
        case HitWall => println("hit wall")
        case Moved if !states.map(_._1).contains(eastPoint) =>
          states = states :+ (eastPoint, goingEast)
        case Moved => println(s"already visited")
        case FoundOxygen =>
          println(s"found it! $eastPoint")
          states = Nil
          return 0
      }
    }

    0
  }


  println(repairDroid(this.input().next()))
}


object Prob15Types {

  sealed trait Command {
    val rep: Long
  }
  case object MoveNorth extends Command {
    override val rep: Long = 1
  }
  case object MoveSouth extends Command {
    override val rep: Long = 2
  }
  case object MoveWest extends Command {
    override val rep: Long = 3
  }
  case object MoveEast extends Command {
    override val rep: Long = 4
  }
  object Command {
    def apply(cmd: Long): Command = cmd match {
      case 1 => MoveNorth
      case 2 => MoveSouth
      case 3 => MoveWest
      case 4 => MoveEast
      case _ => ???
    }
  }


  sealed trait RepairDroidStatus {
    val rep: Long
  }
  case object HitWall extends RepairDroidStatus {
    override val rep: Long = 0
  }
  case object Moved extends RepairDroidStatus {
    override val rep: Long = 1
  }
  case object FoundOxygen extends RepairDroidStatus {
    override val rep: Long = 2
  }
  object RepairDroidStatus {
    def apply(status: Long): RepairDroidStatus = status match {
      case 0 => HitWall
      case 1 => Moved
      case 2 => FoundOxygen
      case _ => ???
    }
  }
}

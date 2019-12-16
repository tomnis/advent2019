package org.mccandless.advent

import org.mccandless.advent.Prob15Types._
import org.mccandless.advent.geometry.Point
import org.mccandless.advent.intcode.{Machine, MachineState}
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

  case class OxygenEmitterStatus(m: Machine, p: Point, distFromOrigin: Long)
  def findOxygenEmitter(program: Program): OxygenEmitterStatus = {
    val m = Machine(program)
    val distancesTo: mutable.Map[Point, Long] = mutable.Map.empty.withDefaultValue(Long.MaxValue)
    distancesTo += (Point(0,0) -> 0L)

    var states: List[(Point, Machine)] = List((Point(0,0),m))
    while(states.nonEmpty) {
      val (curPoint, curMachine) = states.head
      states = states.tail
      val newDist = distancesTo(curPoint) + 1
      println(s"at $curPoint (${states.size} pending states)")


      val inputs = Seq(MoveNorth, MoveSouth, MoveWest, MoveEast)
      inputs.foreach { input =>
        val newMachine: Machine = curMachine.snapshot()
        val step: MachineState = newMachine.run(input.rep)
        val newPoint = curPoint + input.diff

        DroidStatus(step.output) match {
          case HitWall => println(s"hit wall at $newPoint")
          case Moved if !states.map(_._1).contains(newPoint) && newDist < distancesTo(newPoint) =>
            println(s"moved to $newPoint")
            states = states :+ (newPoint, newMachine)
            distancesTo(newPoint) = newDist
          case Moved =>
            println(s"already visited $newPoint")
          case FoundOxygen =>
            println(s"found oxygen emitter at $newPoint")
            return OxygenEmitterStatus(newMachine, newPoint, newDist)
        }
      }
    }
    ???
  }


  val part1 = findOxygenEmitter(this.input().next())
  println(part1.distFromOrigin)



  // part 2
  // diagonal locations are not adjacent.
  // Use the repair droid to get a complete map of the area. How many minutes will it take to fill with oxygen?
  // start exploring from oxygen location
  // longest leaf path is number of minutes
  // we need droid in starting state of oxygen emitter, so need machine from part 1

  def minutesToFill(status: OxygenEmitterStatus): Long = {
    val m = status.m
    val distancesTo: mutable.Map[Point, Long] = mutable.Map.empty.withDefaultValue(Long.MaxValue)
    val startingPoint: Point = status.p
    distancesTo += (startingPoint -> 0L)

    var states: List[(Point, Machine)] = List((startingPoint,m))
    while(states.nonEmpty) {
      val (curPoint, curMachine) = states.head
      states = states.tail
      val newDist = distancesTo(curPoint) + 1
      println(s"at $curPoint (${states.size} pending states)")

      val inputs = Seq(MoveNorth, MoveSouth, MoveWest, MoveEast)
      inputs.foreach { input =>
        val newMachine: Machine = curMachine.snapshot()
        val step: MachineState = newMachine.run(input.rep)
        val newPoint = curPoint + input.diff

        DroidStatus(step.output) match {
          case HitWall => println(s"hit wall at $newPoint")
          case Moved if !states.map(_._1).contains(newPoint) && newDist < distancesTo(newPoint) =>
            println(s"moved to $newPoint")
            states = states :+ (newPoint, newMachine)
            distancesTo(newPoint) = newDist
          case Moved | FoundOxygen =>
            println(s"already visited $newPoint")
        }
      }
    }
    distancesTo.maxBy(_._2)._2
  }

  println(minutesToFill(part1))
}


object Prob15Types {

  sealed trait Command {
    val rep: Long
    val diff: Point
  }
  case object MoveNorth extends Command {
    override val rep: Long = 1
    override val diff: Point = Point(0, 1)
  }
  case object MoveSouth extends Command {
    override val rep: Long = 2
    override val diff: Point = Point(0, -1)
  }
  case object MoveWest extends Command {
    override val rep: Long = 3
    override val diff: Point = Point(-1, 0)
  }
  case object MoveEast extends Command {
    override val rep: Long = 4
    override val diff: Point = Point(1, 0)
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


  sealed trait DroidStatus
  case object HitWall extends DroidStatus
  case object Moved extends DroidStatus
  case object FoundOxygen extends DroidStatus
  object DroidStatus {
    def apply(status: Long): DroidStatus = status match {
      case 0 => HitWall
      case 1 => Moved
      case 2 => FoundOxygen
      case _ => ???
    }
  }
}

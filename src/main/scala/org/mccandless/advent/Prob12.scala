package org.mccandless.advent

import org.mccandless.advent.Prob12Types._
import org.mccandless.advent.util.Parser
import org.mccandless.advent.ntheory.NumberTheory.lcm

import scala.collection.mutable
import scala.math.abs

object Prob12 extends Parser[Moon] with App {

  override val inputFileName: String = "prob12_input.txt"

  override def parse(line: String): Moon = {
    val noBrackets: String = line.tail.dropRight(1)

    val components: Seq[String] = noBrackets.split((", "))

    val x: Long = components.head.drop(2).toLong
    val y: Long = components(1).drop(2).toLong
    val z: Long = components(2).drop(2).toLong

    Moon(Point3(x,y,z))
  }


  require(parse("<x=4, y=-17, z=-12>") == Moon(Point3(4, -17, -12)))


  // input: position of each moon
  // each moon has 3d position and 3d velocity (starts at 0)
  val moons: Seq[Moon] = this.input().toSeq




  // if Ganymede has an x position of 3, and Callisto has a x position of 5,
  // then Ganymede's x velocity changes by +1 (because 5 > 3)
  // and Callisto's x velocity changes by -1 (because 3 < 5).
  // However, if the positions on a given axis are the same,
  // the velocity on that axis does not change for that pair of moons
  def newVal(c1: Long, c2: Long): (Long, Long) = {
    if (c1 < c2) (1, -1)
    else if (c1 > c2) (-1, 1)
    else (0, 0)
  }
  require(newVal(3, 5) == (1, -1))
  require(newVal(5, 3) == (-1, 1))
  require(newVal(3, 3) == (0, 0))


  //
  def velocityDiff(p1: Point3, p2: Point3): (Velocity, Velocity) = {
    val (newx1, newx2) = newVal(p1.x, p2.x)
    val (newy1, newy2) = newVal(p1.y, p2.y)
    val (newz1, newz2) = newVal(p1.z, p2.z)

    (Velocity(newx1, newy1, newz1), Velocity(newx2, newy2, newz2))
  }


  // applies gravity to update velocities
  def applyGravity(moons: Seq[Moon]): Seq[Moon] = {
    val newMoons: mutable.Buffer[Moon] = mutable.Buffer.empty ++ moons.map(_.copy())

    // consider every pair of moons
    // on each axis, velocity changes by exactly +1 or -1
    val moonsAndIndices = moons.zipWithIndex


    moonsAndIndices.foreach { case (m1, idx1) =>
      moonsAndIndices.foreach { case (m2, idx2) =>
        if (idx1 != idx2) {
          val newVs: (Velocity, Velocity) = velocityDiff(m1.p, m2.p)

          // update moons
          val oldM1: Moon = newMoons(idx1)
          val newM1: Moon = oldM1.copy(v = oldM1.v + newVs._1)
          newMoons(idx1) = newM1
        }
      }
    }

    newMoons.toSeq
  }




  def applyVelocity(moons: Seq[Moon]): Seq[Moon] = moons.map(m => m.copy(p = m.p + m.v))


  def totalEnergy(moons: Seq[Moon]): Long = moons.map(_.totalEnergy).sum

  // simulate motion of moon in timesteps
  // within each timestep
  //   update each velocity by applying gravity
  //   update position by applying velocity
  def part1(moons: Seq[Moon]): Long = {
    var currentMoons: Seq[Moon] = moons

    val timeStepLimit: Int = 1000

    (0 until timeStepLimit).foreach { timestep =>
      println(s"after $timestep timesteps")
      currentMoons.foreach(println)
      println("\n")


      currentMoons = applyGravity(currentMoons)
      currentMoons = applyVelocity(currentMoons)
    }

    println(s"after $timeStepLimit timesteps")
    currentMoons.foreach(println)
    println("\n")

    totalEnergy(currentMoons)
  }


  def statex(moons: Seq[Moon]): Seq[Long] = moons.flatMap(m => Seq(m.p.x, m.v.x))
  def statey(moons: Seq[Moon]): Seq[Long] = moons.flatMap(m => Seq(m.p.y, m.v.y))
  def statez(moons: Seq[Moon]): Seq[Long] = moons.flatMap(m => Seq(m.p.z, m.v.z))

  // period along a single dimension
  def period(moons: Seq[Moon], state: Seq[Moon] => Seq[Long]): Long = {
    val seenStates: mutable.Set[Seq[Long]] = mutable.Set(state(moons))

    var currentMoons: Seq[Moon] = moons
    var halt = false
    var stepcounter: Long = 0

    while(!halt) {
      currentMoons = applyGravity(currentMoons)
      currentMoons = applyVelocity(currentMoons)
      stepcounter += 1

      val newState = state(currentMoons)

      halt = seenStates contains newState
      seenStates += newState

      if (stepcounter % 10000 == 0) {
        println(s"finished step $stepcounter")
      }
    }

    stepcounter
  }


  // do the moons ever return to a previous state?
  // how many steps must occur before positions and velocities match a previous point in time
  def part2(moons: Seq[Moon]): Long = {
    val xPeriod = period(moons, statex)
    val yPeriod = period(moons, statey)
    val zPeriod = period(moons, statez)

    println(s"$xPeriod, $yPeriod, $zPeriod")

    lcm(xPeriod, yPeriod, zPeriod)
  }

  println(part1(moons))
  println(part2(moons))
}




object Prob12Types {

  case class Point3(x: Long, y: Long, z: Long) {
    def +(other: Velocity): Point3 = Point3(this.x + other.x, this.y + other.y, this.z + other.z)
    def potentialEnergy: Long = abs(x) + abs(y) + abs(z)
  }

  case class Velocity(x: Long, y: Long, z: Long) {
    def +(other: Velocity): Velocity = Velocity(this.x + other.x, this.y + other.y, this.z + other.z)
    def kineticEnergy: Long = abs(x) + abs(y) + abs(z)
  }

  object Velocity {
    def zero: Velocity = Velocity(0, 0, 0)
  }

  case class Moon(p: Point3, v: Velocity = Velocity.zero) {
    // potential energy * kinetic energy
    def totalEnergy: Long = p.potentialEnergy * v.kineticEnergy
  }
}
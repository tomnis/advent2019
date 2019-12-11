package org.mccandless.advent

import Prob10Types._
import org.mccandless.advent.geometry.Point
import org.mccandless.advent.util.Parser

import scala.collection.mutable

object Prob10 extends Parser[MapRow] with App {
  override val inputFileName = "prob10_input.txt"

  override def parse(line: String): MapRow = line.toCharArray.toSeq.map(Cell(_))

  val mapInput: Seq[MapRow] = this.input().toSeq

  val map: AsteroidMap = (for {
    (row, y) <- mapInput.zipWithIndex
    (cell, x) <- row.zipWithIndex
  } yield (Point(x, y), cell)).toMap

  // tracking asteroids
  // map of all asteroids in region
  // which asteroid would be the best place to build monitoring station?
  // (can detect most number of other asteroids)
  // can detect asteroid if there is direct line of sight (no other asteroid between)

  def visibleAsteroids(map: AsteroidMap, start: Point): Seq[Point] = {
    map.filter(_._2 == Asteroid).view.filterKeys(p => isVisible(map, start, p)).keys.toSeq
  }

  def numVisibleAsteroids(map: AsteroidMap, start: Point): Int = visibleAsteroids(map, start).length


  def isVisible(map: AsteroidMap, p1: Point, p2: Point): Boolean = {
    if (p1 == p2) false
    else p1.rangeTo(p2).tail.dropRight(1).forall(map(_) == Empty)
  }


  // number of detectable asteroids from best asteroid
  def part1(map: AsteroidMap): (Point, Int) = {
    val asteroids: Map[Point, Cell] = map.filter(_._2 == Asteroid)
    val numAsteroids = asteroids.zip(asteroids.map(a => numVisibleAsteroids(map, a._1)))
    val result: ((Point, Cell), Int) = numAsteroids.maxBy(_._2)
    (result._1._1, result._2)
  }

  val soln1: (Point, Int) = part1(map)
  println(s"part1 solution: $soln1")


  // vaporize some asteroids
  // laser starts pointing up, rotates clockwise
  // same asteroids that can be detected can be vaporized

  def sortVisibleAsteroids(visible: Seq[Point], stationLocation: Point): Seq[Point] = {
   visible.sortBy(stationLocation.angle)(Ordering[Double].reverse)
  }


  // what will be 200th asteroid vaporized?
  // x * 100 + y
  def part2(map: AsteroidMap, stationLocation: Point): Point = {

    var numVaporizedAsteroids: Int = 0
    val mutableMap: mutable.Map[Point, Cell] = mutable.Map.empty ++ map

    while (numVaporizedAsteroids != 200 || mutableMap.values.count(_ != Empty) > 1) {
      // get detected asteroids
      val curVisibleAsteroids: Seq[Point] = visibleAsteroids(mutableMap.toMap, stationLocation)
      val sortedVisible: Seq[Point] = sortVisibleAsteroids(curVisibleAsteroids, stationLocation)

      sortedVisible.foreach { as =>
        // vaporized!
        mutableMap(as) = Empty
        numVaporizedAsteroids += 1
        if (numVaporizedAsteroids == 200) {
          return as
        }
      }
    }

    ???
  }

  val soln2 = part2(map, soln1._1)
  println(s"part2 solution: ${soln2.x * 100 + soln2.y}")
}


object Prob10Types {
  sealed trait Cell
  case object Empty extends Cell
  case object Asteroid extends Cell

  object Cell {
    def apply(c: Char): Cell = c match {
      case '.' => Empty
      case '#' => Asteroid
    }
  }

  type MapRow = Seq[Cell]
  type AsteroidMap = Map[Point, Cell]
}

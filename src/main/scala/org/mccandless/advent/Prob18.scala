package org.mccandless.advent

import org.mccandless.advent.geometry.Point
import org.mccandless.advent.util.Parser
import Prob18Types._

import scala.collection.{immutable, mutable}

object Prob18 extends Parser[String] with App {

  override val inputFileName = "prob18_input.txt"
  override def parse(line: String): String = line


  def toGrid(lines: Seq[String]): Grid = {
    val cells = for {
      (line, row) <- lines.zipWithIndex
      (elem, col) <- line.zipWithIndex
    } yield (Point(col, row) -> elem)
    cells.toMap
  }
  require(toGrid(Seq(
    "#########",
    "#b.A.@.a#",
    "#########"
  ))(Point(3,1)) == 'A')


  def getCurPosition(grid: Grid): Point = {
    val x = grid.filter(_._2 == entrance)
    require(x.size == 1)
    x.keys.head
  }
  require(getCurPosition(toGrid(Seq(
    "#########",
    "#b.A.@.a#",
    "#########"
  ))) == Point(5,1))


  val smallGrid1: Grid = toGrid(Seq(
    "########################",
    "#f.D.E.e.C.b.A.@.a.B.c.#",
    "######################.#",
    "#d.....................#",
    "########################"
  ))


  def isKey(c: Char): Boolean = c.isLetter && c.isLower
  require(isKey('a'))
  require(!isKey('A'))


  def isDoor(c: Char): Boolean = c.isLetter && c.isUpper
  require(!isDoor('a'))
  require(isDoor('A'))

  def getKeys(grid: Grid): Grid = grid.filter { case (_, value) => isKey(value) }
  def getDoors(grid: Grid): Grid = grid.filter { case (_, value) => isDoor(value) }


  def getDoorPos(grid: Grid, key: Char): Point = {
    val k = getKeys(grid).filter(_._2 == key)
    require(k.size == 1)
    val l = getDoors(grid).filter(_._2 == key.toUpper)
    require(l.size == 1)
    l.keys.head
  }


  // return None if there is no path
  def shortestPathTo(grid: Grid, start: Point, end: Point): Option[Long] = {
    val distancesTo: mutable.Map[Point, Long] = mutable.Map.empty.withDefaultValue(Long.MaxValue)
    distancesTo += (start -> 0L)

    var states: List[Point] = List(start)

    while (states.nonEmpty) {
      val curPos = states.head
      states = states.tail

      val newDist = distancesTo(curPos) + 1

      Seq(MoveNorth, MoveSouth, MoveWest, MoveEast).foreach { move =>

        val newPos = curPos + move.diff

        grid(newPos) match {
          case _ if newPos == end =>
            return Option(newDist)
          case '#' =>
          case '.' if !states.contains(newPos) && newDist < distancesTo(newPos) =>
            states = states :+ newPos
            distancesTo(newPos) = newDist
            // blocked
          case _ =>
        }
      }
    }


    distancesTo.get(end)
  }
  require(shortestPathTo(smallGrid1, Point(15, 1), Point(17, 1)).contains(2))
  require(shortestPathTo(smallGrid1, Point(15, 1), Point(18, 1)).isEmpty)


  // a massive underground vault.
  // You generate a map of the tunnels (your puzzle input).
  // The tunnels are too narrow to move diagonally

  def part1(grid: Grid): Long = {

    // collecting the key unlocks corresponding door

    // get all the keys that have an open path

    val distancesTo: mutable.Map[Grid, Long] = mutable.Map.empty.withDefaultValue(Long.MaxValue)
    distancesTo += (grid -> 0L)

    var states: List[Grid] = List(grid)
    while(states.nonEmpty) {
      val curGrid = states.head
      states = states.tail
      val curPos: Point = getCurPosition(curGrid)

      // get the keys we can move to
      val keys = getKeys(curGrid)

      // foreach key with an open path
      val paths: Seq[Option[(Point, Long)]] = keys.map { case (point, _) =>
        shortestPathTo(curGrid, curPos, point).map { length => (point, length) }
      }.toSeq

      println(s"num remaining keys: ${paths.length}")

      // copy the grid
      paths.flatten.foreach { case (keyLocation, length) =>
        val key: Char = curGrid(keyLocation)
        val newDist: Long = distancesTo(curGrid) + length

//        println(s"current position: $curPos, checking dest $keyLocation (key=$key, length=$length)")
        var newGrid: Grid = Map.empty ++ curGrid
        // grid(key) == entrance
        newGrid = newGrid.updated(keyLocation, entrance)
        // move our position to key position
        newGrid = newGrid.updated(curPos, passage)
        // unlock the door
        // grid(door(key)) == passage
        newGrid = newGrid.updated(getDoorPos(curGrid, key), passage)

        // if we have found a shorter path to this new grid state, queue it up and update distancesTo
        if (!states.contains(newGrid) && newDist < distancesTo(newGrid)) {
//          println(s"found a shorter path")
          states = states :+ newGrid
          distancesTo(newGrid) = newDist
        }
      }
    }


    distancesTo.filter(_._1.values.toSet.size == 3).head._2
  }


  println(part1(this.toGrid(this.input().toSeq)))
}


object Prob18Types {

  type Grid = Map[Point, Char]

  val entrance: Char = '@'
  val passage: Char = '.'
  val wall: Char = '#'
  // keys are lowecase letters
  // doors are uppercase letters


  sealed trait Move {
    val diff: Point
  }
  case object MoveNorth extends Move {
    override val diff: Point = Point(0, -1)
  }
  case object MoveSouth extends Move {
    override val diff: Point = Point(0, 1)
  }
  case object MoveWest extends Move {
    override val diff: Point = Point(-1, 0)
  }
  case object MoveEast extends Move {
    override val diff: Point = Point(1, 0)
  }
}

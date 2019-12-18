package org.mccandless.advent

import org.mccandless.advent.geometry.Point
import org.mccandless.advent.util.Parser
import Prob18Types._

import scala.collection.mutable

object Prob18 extends Parser[String] with App {

  override val inputFileName = "prob18_input.txt"
  override def parse(line: String): String = line


  def toGrid(lines: Seq[String]): Grid = {
    val cells = for {
      (line, row) <- lines.zipWithIndex
      (elem, col) <- line.toCharArray.zipWithIndex
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
    "#########",
    "#b.A.@.a#",
    "#########"
  ))


  val smallGrid2: Grid = toGrid(Seq(
    "########################",
    "#f.D.E.e.C.b.A.@.a.B.c.#",
    "######################.#",
    "#d.....................#",
    "########################"
  ))


  val smallGrid3: Grid = toGrid(Seq(
    "########################",
    "#...............b.C.D.f#",
    "#.######################",
    "#.....@.a.B.c.d.A.e.F.g#",
    "########################"
  ))


  val smallGrid4: Grid = toGrid(Seq(
    "#################",
    "#i.G..c...e..H.p#",
    "########.########",
    "#j.A..b...f..D.o#",
    "########@########",
    "#k.E..a...g..B.n#",
    "########.########",
    "#l.F..d...h..C.m#",
    "#################"
  ))


  val smallGrid5: Grid = toGrid(Seq(
    "########################",
    "#@..............ac.GI.b#",
    "###d#e#f################",
    "###A#B#C################",
    "###g#h#i################",
    "########################"
  ))


  def isKey(c: Char): Boolean = c.isLetter && c.isLower
  require(isKey('a'))
  require(!isKey('A'))
  require(!isKey('#'))
  require(!isKey('.'))


  def isDoor(c: Char): Boolean = c.isLetter && c.isUpper
  require(!isDoor('a'))
  require(isDoor('A'))
  require(!isDoor('#'))
  require(!isDoor('.'))

  def getKeys(grid: Grid): Grid = grid.filter { case (_, value) => isKey(value) }
  def getDoors(grid: Grid): Grid = grid.filter { case (_, value) => isDoor(value) }


  def getDoorPos(grid: Grid, key: Char): Option[Point] = {
    val k = getKeys(grid).filter(_._2 == key)
    require(k.size == 1)
    val l = getDoors(grid).filter(_._2 == key.toUpper)
    // its possible that this key has no corresponding door
    require(l.size <= 1)
    l.keys.headOption
  }


  def getValidMoves(grid: Grid, startPos: Point, acquiredKeys: Set[Char]): Seq[(Point, Long)] = {

    var curPos = startPos
    var states: List[Point] = List(curPos)
    val distancesTo: mutable.Map[Point, Long] = mutable.Map.empty.withDefaultValue(Long.MaxValue)
    distancesTo += (curPos -> 0L)

    while (states.nonEmpty) {
      curPos = states.head
      states = states.tail
      val newDist = distancesTo(curPos) + 1

      Seq(MoveNorth, MoveSouth, MoveWest, MoveEast).foreach { move =>

        val newPos = curPos + move.diff
//        println(s"new pos: $newPos   ${grid(newPos)}")
        grid(newPos) match {
            // we found a key location, but cant move any further
          case key if isKey(key) =>
            if (newDist < distancesTo(newPos)) {
              distancesTo(newPos) = newDist
              if (acquiredKeys.contains(key)) {
                states = states :+ newPos
              }
            }

          case door if isDoor(door) =>
            if (newDist < distancesTo(newPos)) {
              distancesTo(newPos) = newDist
              if (acquiredKeys.contains(door.toLower)) {
                states = states :+ newPos
              }
            }
            // empty space, we should check our dist and proceed
          case '.' | '@' if newDist < distancesTo(newPos) =>
            states = states :+ newPos
            distancesTo(newPos) = newDist
          // wall, we are blocked
          case _ =>
        }
      }
    }

    distancesTo.toSeq.filter { case (point, _) =>
      isKey(grid(point)) && !acquiredKeys.contains(grid(point))
    }
  }





  // a massive underground vault.
  // You generate a map of the tunnels (your puzzle input).
  // The tunnels are too narrow to move diagonally

  def part1(grid: Grid): Long = {

    type State = (Point, Set[Char])
    // collecting the key unlocks corresponding door

    val allKeys = this.getKeys(grid)
    println(s"need to acquire ${allKeys.size} keys")
    // get all the keys that have an open path
    val startState = (getCurPosition(grid), Set.empty[Char])
    val distancesTo: mutable.Map[State, Long] = mutable.Map.empty.withDefaultValue(Long.MaxValue)
    distancesTo += (startState -> 0L)
    var states: List[State] = List(startState)

    while(states.nonEmpty) {
      val (curPos, keys) = states.head
      states = states.tail

      // foreach key with an open path
      val paths: Seq[(Point, Long)] = getValidMoves(grid, curPos, keys)

      println(s"num valid moves: ${paths.length} $paths")

      // copy the grid
      paths.foreach { case (keyLocation, length) =>
//        println(s"acquiring key $keyLocation ${grid(keyLocation)}")
        val newDist: Long = distancesTo((curPos, keys)) + length

//        println(s"current position: $curPos, checking dest $keyLocation (key=$key, length=$length)")
        // grid(key) == entrance
        val newKeys = keys + grid(keyLocation)
        val newPos = keyLocation
        // move our position to key position
        // unlock the door
        // grid(door(key)) == passage
        // there may not be a door corresponding to this key

        val newState = (newPos, newKeys)
        // if we have found a shorter path to this new grid state, queue it up and update distancesTo
        if (newDist < distancesTo(newState)) {
//          println(s"found a shorter path")
          states = states :+ newState
          distancesTo(newState) = newDist
        }
      }
    }


    distancesTo.filter(_._1._2.size == allKeys.size).values.min
  }


  require(part1(this.smallGrid1) == 8)
  println("passed check 1")
  require(part1(this.smallGrid2) == 86)
  println("passed check 2")
  require(part1(this.smallGrid3) == 132)
  println("passed check 3")
  require(part1(this.smallGrid4) == 136)
  println("passed check 4")
  require(part1(this.smallGrid5) == 81)
  println("passed check 5")

  println(part1(this.toGrid(this.input().toSeq)))
  // 4248 correct
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

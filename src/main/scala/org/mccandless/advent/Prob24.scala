package org.mccandless.advent

import org.mccandless.advent.Prob24Types.{Bug, Cell, Empty, Grid, RecGrid, RecPoint}
import org.mccandless.advent.geometry.Point
import org.mccandless.advent.util.Parser

import scala.collection.mutable

/**
 *
 * Created by tdm on 12/23/19.
 */
object Prob24 extends Parser[String] with App {

  override val inputFileName = "prob24_input.txt"
  override def parse(line: String): String = line

  def toGrid(lines: Seq[String]): Grid = {
    val cells = for {
      (line, row) <- lines.zipWithIndex
      (elem, col) <- line.toCharArray.zipWithIndex
    } yield (Point(col, row) -> Cell(elem))
    cells.toMap
  }

  def renderMap(map: Grid): Unit = {
    val xMin = map.keys.minBy(_.x).x
    val xMax = map.keys.maxBy(_.x).x
    val yMin = map.keys.minBy(_.y).y
    val yMax = map.keys.maxBy(_.y).y

    for (x <- xMin to xMax) { print(if (x / 10 > 0) x / 10 else " ") }
    println()
    for (x <- xMin to xMax) { print(x % 10) }
    println()
    for (y <- yMin to yMax) {
      for (x <- xMin to xMax) {
        print(map(Point(x,y)).rep)
      }
      println(s"  $y")
    }
  }

  def numAdjacentBugs(grid: Grid, p: Point): Long = {
    List(p.up, p.down, p.left, p.right).count(p =>
      grid.withDefaultValue(Empty)(p) == Bug
    )
  }


  /**
   * A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
   *
   * An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it
   *
   * @param oldGrid
   * @return
   */
  def next(oldGrid: Grid): Grid = {
    val og = oldGrid.withDefaultValue(Empty)

    og.map {
      case (p, Bug) =>
        if (numAdjacentBugs(og, p) == 1)  (p, Bug)
        else (p, Empty)
      case (p, Empty) =>
        val n = numAdjacentBugs(og, p)
        if (n == 1 || n == 2) (p, Bug)
        else (p, Empty)
    }
  }

  /**
   * consider each tile left-to-right in the top row,
   * then left-to-right in the second row, and so on.
   * Each of these tiles is worth biodiversity points equal to increasing powers of two:
   *   1, 2, 4, 8, 16, 32, and so on.
   * Add up the biodiversity points for tiles with bugs
   * @param grid
   * @return
   */
  def bioDiversityRating(grid: Grid): Long = {
    val bugs = grid.filter(_._2 == Bug)

    bugs.keys.map { p =>
      val pow = p.y * 5 + p.x
      Math.pow(2, pow)
    }.sum.toLong
  }


  val m = toGrid(this.input().toSeq)
  renderMap(m)


  def part1(grid: Grid): Long = {
    val seen: mutable.Set[Grid] = mutable.Set(grid)
    var oldGrid = grid
    while (true) {
      val newGrid = next(oldGrid)
      renderMap(newGrid)

      if (seen.contains(newGrid)) {
        return bioDiversityRating(newGrid)
      }

      seen += newGrid
      oldGrid = newGrid
//      Thread.sleep(2000)
    }
    0
  }

//  println(part1(m))

  /**


     |     |         |     |
  1  |  2  |    3    |  4  |  5
     |     |         |     |
-----+-----+---------+-----+-----
     |     |         |     |
  6  |  7  |    8    |  9  |  10
     |     |         |     |
-----+-----+---------+-----+-----
     |     |A|B|C|D|E|     |
     |     |-+-+-+-+-|     |
     |     |F|G|H|I|J|     |
     |     |-+-+-+-+-|     |
 11  | 12  |K|L|?|N|O|  14 |  15
     |     |-+-+-+-+-|     |
     |     |P|Q|R|S|T|     |
     |     |-+-+-+-+-|     |
     |     |U|V|W|X|Y|     |
-----+-----+---------+-----+-----
     |     |         |     |
 16  | 17  |    18   |  19 |  20
     |     |         |     |
-----+-----+---------+-----+-----
     |     |         |     |
 21  | 22  |    23   |  24 |  25
     |     |         |     |


   */

  val center: Point = Point(2,2)
  val topCenter: Point = Point(2,1)
  val bottomCenter: Point = Point(2,3)
  val leftCenter: Point = Point(1,2)
  val rightCenter: Point = Point(3,2)

  val topRow: Seq[Point] = (0 to 4).map(i => Point(i, 0))
  val bottomRow: Seq[Point] = (0 to 4).map(i => Point(i, 4))
  val leftCol: Seq[Point] = (0 to 4).map(i => Point(0, i))
  val rightCol: Seq[Point] = (0 to 4).map(i => Point(4, i))



  def getNeighbors(recP: RecPoint): Seq[RecPoint] = {
    val point = recP.point
    val outerLevel: Long = recP.level - 1
    val innerLevel: Long = recP.level + 1

   (point.x, point.y) match {
      // shouldnt ever happen
      case (2, 2) => ???

        // top left corner
      case (0, 0) =>
        Seq(recP.right, recP.down) ++ Seq(topCenter, leftCenter).map(RecPoint(outerLevel, _))
        // top right corner
      case (4, 0) =>
        Seq(recP.left, recP.down) ++ Seq(topCenter, rightCenter).map(RecPoint(outerLevel, _))
        // bottom left corner
      case (0, 4) =>
        Seq(recP.up, recP.right) ++ Seq(leftCenter, bottomCenter).map(RecPoint(outerLevel, _))
        // bottom right corner
      case (4, 4) =>
        Seq(recP.up, recP.left) ++ Seq(rightCenter, bottomCenter).map(RecPoint(outerLevel, _))


      // if we are in the
      // add top row from the inner nested grid
      case (2, 1) =>
        Seq(recP.up, recP.left, recP.right) ++  topRow.map(p => RecPoint(innerLevel, p))
      // add bottom row from inner nested grid
      case (2, 3) =>
        Seq(recP.down, recP.left, recP.right) ++ bottomRow.map(p => RecPoint(innerLevel, p))
      // add left row from inner nested grid
      case (1, 2) =>
        Seq(recP.down, recP.left, recP.up) ++ leftCol.map(p => RecPoint(innerLevel, p))
      // add right from inner nested grid
      case (3, 2) =>
        Seq(recP.down, recP.up, recP.right) ++ rightCol.map(p => RecPoint(innerLevel, p))

        // add outer levels
      case (x, 0) => // add (2,1) on outer level
        Seq(recP.left, recP.right, recP.down, RecPoint(outerLevel, topCenter))
      case (x, 4) => // add (2,3) on outer level
        Seq(recP.left, recP.right, recP.up, RecPoint(outerLevel, bottomCenter))
      case (0, y) => // add (1,2) on outer level
        Seq(recP.up, recP.down, recP.right, RecPoint(outerLevel, leftCenter))
      case (4, y) => // add (3,2) on outer level
        Seq(recP.up, recP.down, recP.left, RecPoint(outerLevel, rightCenter))

        // everything within our own level
      case _ =>
        Seq(recP.up, recP.down, recP.left, recP.right)
    }
  }


  println(getNeighbors(RecPoint(0, Point(3,3))))
  println(getNeighbors(RecPoint(0, Point(1,1))))
  println(getNeighbors(RecPoint(0, Point(0,0))))
  println(getNeighbors(RecPoint(0, Point(3,2))))


  def numAdjacentBugsRec(grid: RecGrid, point: RecPoint): Long = {
    val neighbors: Seq[RecPoint] = getNeighbors(point)
    neighbors.map(grid.withDefaultValue(Empty)(_)).count(_ == Bug)
  }



  def nextRecGrid(grid: RecGrid): RecGrid = {
    val bugs: Set[RecPoint] = grid.filter(_._2 == Bug).keys.toSet
    val bugNeighbors: Set[RecPoint] = bugs.flatMap(getNeighbors)

    bugNeighbors.map { neighbor =>
      grid.withDefaultValue(Empty)(neighbor) match {
        case Bug =>
          if (numAdjacentBugsRec(grid, neighbor) == 1)  (neighbor, Bug)
          else (neighbor, Empty)

        case Empty =>
          val n = numAdjacentBugsRec(grid, neighbor)
          if (n == 1 || n == 2) (neighbor, Bug)
          else (neighbor, Empty)
      }
    }.toMap
  }



  def part2(grid: Grid): Long = {
    // inner level 1, outer grid level -1
    // add our level
    var recGrid: RecGrid = grid.map { case (k, v) =>
      (RecPoint(0, k), v)
    }

    println(s"bugs after 0 minutes: ${recGrid.values.count(_ == Bug)}")
    (1 to 200).foreach { minute =>
      val newGrid: RecGrid = nextRecGrid(recGrid)
      val numBugs = newGrid.values.count(_ == Bug)
      val minLevel: Long = newGrid.keys.map(_.level).min
      val maxLevel: Long = newGrid.keys.map(_.level).max
      println(s"bugs after $minute minutes: $numBugs, levels: ($minLevel, $maxLevel)")
      recGrid = newGrid
    }
    0
  }

  // 3412 too high
  // 3525 too high

  val test = Seq(
    "....#",
    "#..#.",
    "#.?##",
    "..#..",
    "#...."
  )
  val testGrid = toGrid(test)

  part2(testGrid)
  part2(toGrid(this.input().toSeq))
}


object Prob24Types {

  case class RecPoint(level: Long, point: Point) {
    def up: RecPoint = copy(point = point.up)
    def down: RecPoint = copy(point = point.down)
    def left: RecPoint = copy(point = point.left)
    def right: RecPoint = copy(point = point.right)
  }
  type Grid = Map[Point, Cell]
  type RecGrid = Map[RecPoint, Cell]

  // The bugs live and die based on the number of bugs in the four adjacent tiles
  sealed trait Cell {
    val rep: Char
  }

  case object Bug extends Cell {
    override val rep: Char = '#'
  }

  case object Empty extends Cell {
    override val rep: Char = '.'
  }

  object Cell {
    def apply(c: Char): Cell = c match {
      case '#' => Bug
      case '.' | '?' => Empty
    }
  }
}
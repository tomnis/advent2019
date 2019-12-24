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
//    for (x <- xMin to xMax) { print(if (x / 10 > 0) x / 10 else " ") }
//    println()
//    for (x <- xMin to xMax) { print(x % 10) }
//    println()
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


  def getNeighbors(grid: RecGrid, p: RecPoint): Seq[RecPoint] = {
    val point = p.point
    val candidates: Seq[RecPoint] = p match {
      // shouldnt ever happen
      case RecPoint(l, Point(2, 2)) => ???

      case RecPoint(l, Point(0, 0)) =>
        Seq(point.right, point.down, )


      // if we are in the
      // add top row from the inner nested grid
      case RecPoint(l, Point(2, 1)) =>
        (Seq(point.up, point.left, point.right) ++ (0 to 4).map(i => Point(i, 0))).map(p => RecPoint(l + 1, p))
      // add bottom row from inner nested grid
      case RecPoint(l, Point(2, 3)) =>
        (Seq(point.down, point.left, point.right) ++ (0 to 4).map(i => Point(i, 4))).map(p => RecPoint(l + 1, p))
      // add left row from inner nested grid
      case RecPoint(l, Point(1, 2)) =>
        (Seq(point.down, point.left, point.up) ++ (0 to 4).map(i => Point(0, i))).map(p => RecPoint(l + 1, p))
      // add right from inner nested grid
      case RecPoint(l, Point(3, 2)) =>
        (Seq(point.down, point.up, point.right) ++ (0 to 4).map(i => Point(4, i))).map(p => RecPoint(l + 1, p))

        // add outer levels
      case RecPoint(l, Point(x, 0)) => // add (2,1) on outer level
        Seq(point.left, point.right, point.down, Point(2,1)).map(p => RecPoint(l - 1, p))
      case RecPoint(l, Point(x, 4)) => // add (2,3) on outer level
        Seq(point.left, point.right, point.up, Point(2,3)).map(p => RecPoint(l - 1, p))
      case RecPoint(l, Point(0, y)) => // add (1,2) on outer level
        Seq(point.up, point.down, point.left, Point(1,2)).map(p => RecPoint(l - 1, p))
      case RecPoint(l, Point(4, y)) => // add (3,2) on outer level
        Seq(point.up, point.down, point.right, Point(3,2)).map(p => RecPoint(l - 1, p))

      case RecPoint(l, _) =>
        Seq(point.up, point.down, point.left, point.right).map(p => RecPoint(l, p)).filter(_.point != Point(2,2))
      // stay within our own level
//      case RecPoint(l, p) => Seq(p.up, p.down, p.left, p.right).map(p => RecPoint(l, p))
    }

    candidates.filter { p =>
        p.point.x >= 0 && p.point.x <= 4 && p.point.y >= 0 && p.point.y <= 4
    }
  }


  def numAdjacentBugsRec(grid: RecGrid, point: RecPoint): Long = {
    val neighbors: Seq[RecPoint] = getNeighbors(grid, point)
    neighbors.map(grid.withDefaultValue(Empty)(_)).count(_ == Bug)
  }



  def nextRecGrid(grid: RecGrid): RecGrid = {
    val minLevel = grid.keys.map(_.level).min
    val maxLevel = grid.keys.map(_.level).max

    val points: Seq[RecPoint] = for {
      level <- (minLevel - 2) to (maxLevel + 2)
      y <- Seq(0,1,3,4)
      x <- Seq(0,1,3,4)
    } yield RecPoint(level, Point(x, y))


    points.map { p =>
      grid.withDefaultValue(Empty)(p) match {
        case Bug =>
          if (numAdjacentBugsRec(grid, p) == 1) (p, Bug)
          else (p, Empty)
        case Empty =>
          val n = numAdjacentBugsRec(grid, p)
          if (n == 1 || n == 2) (p, Bug)
          else (p, Empty)
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
      println(s"bugs after $minute minutes: $numBugs")
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
//  part2(toGrid(this.input().toSeq))
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
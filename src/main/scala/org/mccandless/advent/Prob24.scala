package org.mccandless.advent

import org.mccandless.advent.Prob24Types.{Bug, Cell, Empty, Grid}
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

  println(part1(m))
}


object Prob24Types {

  type Grid = Map[Point, Cell]


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
      case '.' => Empty
    }
  }
}
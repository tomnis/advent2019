package org.mccandless.advent

import org.mccandless.advent.geometry.Point
import org.mccandless.advent.util.Parser
import Prob20Types._

import scala.collection.mutable

object Prob20 extends Parser[String] with App{

  override val inputFileName: String = "prob20_input.txt"
  val inputFileNameSmall1: String = "prob20_input_small1.txt"

  override def parse(line: String): String = line

  def northOf(p: Point): Point = p + Point(0, -1)
  def southOf(p: Point): Point = p + Point(0, 1)
  def eastOf(p: Point): Point = p + Point(1, 0)
  def westOf(p: Point): Point = p + Point(-1, 0)

  def toGrid(lines: Seq[String]): RawInput = {
    val cells = for {
      (line, row) <- lines.zipWithIndex
      (elem, col) <- line.toCharArray.zipWithIndex
    } yield (Point(col, row) -> elem)
    cells.toMap
  }

  def toMaze(input: RawInput): Maze = {

    input.map { case (p, c) =>
      // if c is a letter, we prob dont need it in our maze
      if (c.isLetter) (p -> Wall)
      else if (c == '#') (p -> Wall)
      else if (c == '.') (p -> Passage(getLabel(input, p)))
      else (p -> Wall)
    }
  }


  def getLabel(input: RawInput, p: Point): Option[Label] = {
    val safeInput: RawInput = input.withDefaultValue('_')
    // check all around p
    val n = northOf(p)
    val e = eastOf(p)
    val s = southOf(p)
    val w = westOf(p)

    // A
    // B
    // .
    if (safeInput(n).isLetter) Option(safeInput(northOf(n)) + "" + safeInput(n))
    // .DE
    else if (safeInput(e).isLetter) Option(safeInput(e) + "" + safeInput(eastOf(e)))
    // .
    // A
    // B
    else if (safeInput(s).isLetter) Option(safeInput(s) + "" + safeInput(southOf(s)))
    // DE.
    else if (safeInput(w).isLetter) Option(safeInput(westOf(w)) + "" + safeInput(w))
    else None
  }


  def getMoves(maze: Maze, cur: Point): Seq[Point] = {
    // check all 4 directions, and check label to teleport
    val moves = List(northOf(cur), eastOf(cur), southOf(cur), westOf(cur)).filter { p =>
      maze(p).isInstanceOf[Passage]
    }

    val maybeLabel = maze(cur).asInstanceOf[Passage].maybeLabel
    if (maybeLabel.isDefined && maybeLabel.get != "AA" && maybeLabel.get != "ZZ") {
      // get the corresponding label
      val pair = maze.filter { case (p, cell) =>
        cell match {
          case Wall => false
          case Passage(secondLabel) if secondLabel.isDefined => secondLabel.get == maybeLabel.get
          case _ => false
        }
      }
      require(pair.size == 2)
      pair.keys.filter(_ != cur).head :: moves
    }
    else {
      moves
    }
  }

  // every maze has start AA, end ZZ
  def getStart(maze: Maze): Point = {
    val r = maze.filter { case (p, cell) =>
      cell match {
        case Passage(label) if label.isDefined => label.get == "AA"
        case _ => false
      }
    }
    require(r.size == 1)
    r.keys.head
  }


    def getEnd(maze: Maze): Point = {
    val r = maze.filter { case (p, cell) =>
      cell match {
        case Passage(label) if label.isDefined => label.get == "ZZ"
        case _ => false
      }
    }
    require(r.size == 1)
    r.keys.head
  }

  // When on an open tile next to one of these labels, a single step can take you to the other tile with the same label.
  // (You can only walk on . tiles; labels and empty space are not traversable.)

  def part1(maze: Maze): Long = {
    val start = getStart(maze)
    val end = getEnd(maze)


    val distancesTo: mutable.Map[Point, Long] = mutable.Map.empty.withDefaultValue(Long.MaxValue)
    distancesTo += (start -> 0L)
    var states: List[Point] = List(start)


    while (states.nonEmpty) {
      val cur = states.head
      println(s"at state $cur")
      states = states.tail


      val moves: Seq[Point] = getMoves(maze, cur)
      moves.foreach { move =>

        val newDist: Long = distancesTo(cur) + 1
        if (newDist < distancesTo(move)) {
          states = move :: states
          distancesTo(move) = newDist
        }
      }
    }


    distancesTo(end)
  }



//  println(part1(toMaze(toGrid(this.input(this.inputFileNameSmall1).toSeq))))
//  println(part1(toMaze(toGrid(this.input(this.inputFileName).toSeq))))



  def getRecMoves(maze: Maze, cur: RecPoint, maxRec: Long): Seq[RecPoint] = {
    // check all 4 directions, and check label to teleport
    val moves: List[RecPoint] = List(northOf(cur.point), eastOf(cur.point), southOf(cur.point), westOf(cur.point)).filter { p =>
      maze(p).isInstanceOf[Passage]
    }.map { p =>
      RecPoint(cur.level, p)
    }


    val maybeLabel = maze(cur.point).asInstanceOf[Passage].maybeLabel



    // if we are at outer level, level 0,
    //   outer labels dont mean anything
    //   inner labels will increment our level
    //
    // check if we are at outer label
    val maxX: Long = maze.keys.maxBy(_.x).x
    val maxY: Long = maze.keys.maxBy(_.y).y
    val atOuterLabel: Boolean = cur.point.x == 2 || cur.point.y == 2 || cur.point.x == maxX - 2 || cur.point.y == maxY - 2



    if (maybeLabel.isDefined && maybeLabel.get != "AA" && maybeLabel.get != "ZZ") {
      // get the corresponding label
      val pair: Map[Point, Cell] = maze.filter { case (p, cell) =>
        cell match {
          case Wall => false
          case Passage(secondLabel) if secondLabel.isDefined => secondLabel.get == maybeLabel.get
          case _ => false
        }
      }
      require(pair.size == 2)

      val portalMove = pair.keys.filter(_ != cur.point).head
      if (cur.level == 0) {
        if (atOuterLabel) moves
        else RecPoint(1, portalMove) :: moves
      }
      // recursion depth limit
      else if (cur.level == maxRec) {
        moves
      }
      else {
        if (atOuterLabel) RecPoint(cur.level - 1, portalMove) :: moves
        else RecPoint(cur.level + 1, portalMove) :: moves
      }
    }
    else {
      moves
    }
  }





  // The marked connections in the maze aren't portals: they physically connect to a larger or smaller copy of the maze.
  // when we enter the maze, we are at outermost level
  // all other outer labeled tiles are effectively walls.
  // At any other level, AA and ZZ count as walls, but the other outer labeled tiles bring you one level outward
  // if we don't have enough recursion levels, there may not be a path
  def part2(maze: Maze): Option[Long] = {
    val maxRec: Long = 28
    val start: RecPoint = RecPoint(0, getStart(maze))
    val end: RecPoint = RecPoint(0, getEnd(maze))


    val distancesTo: mutable.Map[RecPoint, Long] = mutable.Map.empty.withDefaultValue(Long.MaxValue)
    distancesTo += (start -> 0L)
    var states: List[RecPoint] = List(start)


    while (states.nonEmpty) {
      val cur = states.head
//      println(s"at state $cur")
      states = states.tail


      val moves: Seq[RecPoint] = getRecMoves(maze, cur, maxRec)
      moves.foreach { move =>

        val newDist: Long = distancesTo(cur) + 1
        if (newDist < distancesTo(move)) {
          states = move :: states
          distancesTo(move) = newDist
        }
      }
    }


    distancesTo.get(end)
  }

  println(part2(toMaze(toGrid(this.input(this.inputFileNameSmall1).toSeq))))
  println(part2(toMaze(toGrid(this.input(this.inputFileName).toSeq))))
}



object Prob20Types {

  type Label = String

  sealed trait Cell {
    val rep: Char
  }
  case object Wall extends Cell {
    override val rep: Char = '#'
  }
  // label used to traverse
  case class Passage(maybeLabel: Option[Label]) extends Cell {
    override val rep: Char = '.'
  }

  object Cell {
    def apply(c: Char): Cell = c match {
      case '.' => Passage(None)
      case '#' => Wall
    }
    def apply(label: Label): Cell = Passage(Option(label))
  }


  // point together with our recursion level in the maze
  case class RecPoint(level: Long, point: Point)

  type Maze = Map[Point, Cell]


  type RawInput = Map[Point, Char]
}

package org.mccandless.advent.util.cardinal

import org.mccandless.advent.geometry.Point

/**
 *
 * Created by tdm on 12/11/19.
 */
sealed trait Cardinal {

  /** Return the point directly in front of us */
  def front(pt: Point): Point
  /** Return the point directly behind us */
  def back(pt: Point): Point
  /** Return the point directly to our left */
  def left(pt: Point): Point
  /** Return the point directly to our right */
  def right(pt: Point): Point
}

case object North extends Cardinal {
  override def front(pt: Point): Point = pt.up
  override def back(pt: Point): Point = pt.down
  override def left(pt: Point): Point = pt.left
  override def right(pt: Point): Point = pt.right
}


case object South extends Cardinal {
  override def front(pt: Point): Point = pt.down
  override def back(pt: Point): Point = pt.up
  override def left(pt: Point): Point = pt.right
  override def right(pt: Point): Point = pt.left
}


case object East extends Cardinal {
  override def front(pt: Point): Point = pt.right
  override def back(pt: Point): Point = pt.left
  override def left(pt: Point): Point = pt.up
  override def right(pt: Point): Point = pt.down
}


case object West extends Cardinal {
  override def front(pt: Point): Point = pt.left
  override def back(pt: Point): Point = pt.right
  override def left(pt: Point): Point = pt.down
  override def right(pt: Point): Point = pt.up
}


object Cardinal {
  val validChars: Set[Char] = Set('^', 'V', '<', '>')

  def apply(c: Char): Cardinal = c match {
    case '^' => North
    case 'v' => South
    case '>' => East
    case '<' => West
    case _ => ???
  }
}

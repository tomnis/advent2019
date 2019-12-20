package org.mccandless.advent.util.cardinal

/**
 *
 * Created by tdm on 12/11/19.
 */
sealed trait Cardinal
case object North extends Cardinal
case object South extends Cardinal
case object East extends Cardinal
case object West extends Cardinal

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

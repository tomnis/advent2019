package org.mccandless.advent.intcode

// parameter modes
// an output parameter (written to) will never be in immediate mode
sealed trait Mode
case object Position extends Mode
case object Immediate extends Mode
case object Relative extends Mode

object Mode {
  def apply(mode: Long): Mode = mode match {
    case 0 => Position
    case 1 => Immediate
    case 2 => Relative
    case other => throw new IllegalArgumentException(s"unknown mode $other")
  }

  def parse(modes: Long): Seq[Mode] = {
    modes.toString.reverse.toSeq.map(a => Mode(a.toString.toLong)) ++ Seq.fill(10)(Position)
  }
}
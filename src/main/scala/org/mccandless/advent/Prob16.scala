package org.mccandless.advent

import org.mccandless.advent.util.Parser

/**
 * Created by tomas.mccandless on 12/15/19.
 */
object Prob16 extends Parser[Seq[Long]] with App {

  override val inputFileName: String = "prob16_input.txt"

  override def parse(line: String): Seq[Long] = line.toCharArray.toSeq.map(_.toString.toLong)


  /**
   * The base pattern is 0, 1, 0, -1.
   * Then, repeat each value in the pattern a number of times equal to the position in the output list being considered.
   * Repeat once for the first element, twice for the second element, three times for the third element
   * When applying the pattern, skip the very first value exactly once.
   */
  def expandPattern(idx: Int): Seq[Long] = {
    val basePattern: Seq[Long] = Seq(0,1,0,-1)
    basePattern.flatMap(p => Seq.fill(idx + 1)(p))
  }
  require(expandPattern(2) == Seq(0, 0, 0, 1, 1, 1, 0, 0, 0, -1, -1, -1))



  def fftPhase(input: Seq[Long]): Seq[Long] = {
    // Each element in the new list is built by multiplying every value in the input list by a value in a repeating pattern
    // and then adding up the results
    val l = input.length
    require(l % 2 == 0)

    (0 until input.length).map { idx =>
      val pattern: Seq[Long] = expandPattern(idx)
      // repeat pattern
      val pLength = scala.math.ceil(l.toDouble / pattern.length).toInt
      //      println(s"input length: $l, idx: $idx:, pattern length: ${pattern.length} plength: $pLength")
      // drop pattern head
      val appliedPattern: Seq[Long] = Seq.fill(pLength + 1)(pattern).flatten.tail.take(l)

      //      println(s"input length: $l, appliedPatternLength ${appliedPattern.length}")
      // multiply
      require(appliedPattern.length >= input.length)
      val r: Long = input.zip(appliedPattern).foldLeft(0L) { case (a, (in, pat)) =>
        a + ((in * pat) % 10)
      }
      // Then, only the ones digit is kept: 38 becomes 8, -17 becomes 7, and so on
      r.abs % 10
    }
  }
  require(fftPhase(Seq(1,2,3,4,5,6,7,8)) == Seq(4,8,2,2,6,1,5,8))
  require(fftPhase(Seq(4,8,2,2,6,1,5,8)) == Seq(3,4,0,4,0,4,3,8))
  require(fftPhase(Seq(3,4,0,4,0,4,3,8)) == Seq(0,3,4,1,5,5,1,8))
  require(fftPhase(Seq(0,3,4,1,5,5,1,8)) == Seq(0,1,0,2,9,4,9,8))


  def fft(input: Seq[Long], phases: Int): Seq[Long] = {
    //  a new list is constructed with the same length as the input list
    // This new list is also used as the input for the next phase
    var curSignal: Seq[Long] = input

    for (phase <- 1 to phases) {
      curSignal = fftPhase(curSignal)
      println(s"completed $phase phases, prefix = ${curSignal.take(8).mkString("")}")
    }

    println(curSignal)
    curSignal
  }


  def part1(input: Seq[Long]): String = {
    fft(input, 100).take(8).mkString("")
  }
  require(part1(Seq(8,0,8,7,1,2,2,4,5,8,5,9,1,4,5,4,6,6,1,9,0,8,3,2,1,8,6,4,5,5,9,5)) == "24176176")
  require(part1(Seq(1,9,6,1,7,8,0,4,2,0,7,2,0,2,2,0,9,1,4,4,9,1,6,0,4,4,1,8,9,9,1,7)) == "73745418")
  require(part1(Seq(6,9,3,1,7,1,6,3,4,9,2,9,4,8,6,0,6,3,3,5,9,9,5,9,2,4,3,1,9,8,7,3)) == "52432133")

  println(this.input().next().length)
  println(part1(this.input().next()))



  def part2(input: Seq[Long]): String = {
    val phases: Int = 100
    // first seven digits of your initial input signal also represent the message offset
    val offset: Int = input.take(7).mkString("").toInt
    println(s"starting part2, offset=$offset")
    val repeat: Int = 10000
    val realInput: Array[Long] = Array.fill(repeat)(input).flatten

    require(realInput.length % 2 == 0)
    require(offset >= realInput.length / 2)

    1 to phases foreach { phase =>
      var sum: Long = 0

      (realInput.length - 1) to offset by -1 foreach { idx =>
        sum += realInput(idx)
        sum %= 10
        realInput(idx) = sum
      }

      println(s"completed phase $phase")
    }

    realInput.slice(offset, offset + 8).mkString("")
  }

  println(part2(this.input().next()))
}

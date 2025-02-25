package org.mccandless.advent

import org.mccandless.advent.intcode.Machine
import org.mccandless.advent.util.Parser

object Prob5 extends Parser[Array[Long]] with App {

  override val inputFileName = "prob5_input.txt"
  override def parse(line: String): Array[Long] = line.split(",").map(_.toLong)

  val mem: Array[Long] = input().next().clone()

  // input eq 8 (position)
  require(Machine(Array(3,9,8,9,10,9,4,9,99,-1,8)).run().output == 0)
  // input lt 8 (position)
  require(Machine(Array(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)).run().output == 1)
  // input eq 8 (immediate)
  require(Machine(Array(3,3,1108,-1,8,3,4,3,99)).run().output == 0)
  // input lt 8 (immediate)
  require(Machine(Array(3,3,1107,-1,8,3,4,3,99)).run().output == 1)

  // jump checks
  require(Machine(Array(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)).run().output == 1)

  Machine(mem).run()
}

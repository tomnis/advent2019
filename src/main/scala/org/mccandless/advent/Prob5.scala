package org.mccandless.advent

import org.mccandless.advent.intcode.Machine

object Prob5 extends Parser[Array[Int]] with App {

  override val inputFileName = "prob5_input.txt"
  override def parse(line: String): Array[Int] = line.split(",").map(_.toInt)

  val mem: Array[Int] = input().next().clone()

  require(Machine(Array(1002,4,3,4,33)).run().memory == Seq(1002,4,3,4,99))
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

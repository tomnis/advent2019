package org.mccandless.advent

import org.mccandless.advent.intcode.Machine
import org.mccandless.advent.intcode.Types.Program
import org.mccandless.advent.util.Parser

object Prob9 extends Parser[Program] with App {
  override val inputFileName = "prob9_input.txt"

  override def parse(line: String): Program = line.split(",").map(_.toLong)

  val mem: Program = input().next()


  // opcode 9 adjusts relative base

  val sampleInput: Program = Seq(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
  println(Machine(sampleInput.toArray ++ Array.fill(10000)(0L)).run().output)
  val sample2: Program = Seq(1102,34915192,34915192,7,4,7,99,0)
  require(Machine(sample2.toArray ++ Array.fill(10000)(0L)).run().output.toString.length == 16)
  val sample3: Program = Seq(104,1125899906842624L,99)
  require(Machine(sample3.toArray ++ Array.fill(10000)(0L)).run().output == 1125899906842624L)

  // TODO hacky memory adjustment
  Machine(mem.toArray ++ Array.fill(2000)(0L)).run(Seq(1))
  Machine(mem.toArray ++ Array.fill(2000)(0L)).run(Seq(2))
}

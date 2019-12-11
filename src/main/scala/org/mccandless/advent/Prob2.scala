package org.mccandless.advent

import org.mccandless.advent.util.Parser

/**
 *
 * Created by tdm on 2019-12-01.
 */
object Prob2 extends Parser[Array[Int]] with App {

  object Opcodes {

    val ADD: Int = 1
    val MULT: Int = 2
    val HALT: Int = 99
  }
  import this.Opcodes._

  override val inputFileName: String = "prob2_input.txt"

  override def parse(line: String): Array[Int] = line.split(",").map(_.toInt)

  val mem: Array[Int] = input().next().clone()

  def run(memory: Array[Int]): Array[Int] = {
    var pc: Int = 0
    var op: Long = memory(pc)

    while (op != HALT) {
      val in1: Int = memory(memory(pc + 1))
      val in2: Int = memory(memory(pc + 2))
      val out: Int = memory(pc + 3)

      val outVal: Int = op match {
        case ADD => in1 + in2
        case MULT => in1 * in2
      }

      memory(out) = outVal
      pc += 4
      op = memory(pc)
    }

    memory
  }



  require(run(Array(1,0,0,0,99)).toList == List(2,0,0,0,99))
  require(run(Array(2,3,0,3,99)).toList == List(2,3,0,6,99))
  require(run(Array(2,4,4,5,99,0)).toList == List(2,4,4,5,99,9801))
  require(run(Array(1,1,1,4,99,5,6,0,99)).toList == List(30,1,1,4,2,5,6,0,99))


  val out: Array[Int] = run(mem.toList.toArray)

  println(out(0))
  val noun: Int = out(1)
  val verb: Int = out(2)
  println(100 * noun + verb)

  println(out.toList take 3)


  def part2(): Unit = {

    for {
      noun: Int <- 0 to 99
      verb: Int <- 0 to 99
      newInput: Array[Int] = mem.clone()
    } {
      newInput(1) = noun
      newInput(2) = verb
      val newOutput = run(newInput)
      if (newOutput(0) == 19690720) {

        println(s"noun: $noun, verb: $verb, output: ${newOutput(0)}")
        println(100 * noun + verb)
      }
    }
  }

  part2()



}






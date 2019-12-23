package org.mccandless.advent

import org.mccandless.advent.geometry.Point
import org.mccandless.advent.intcode.Machine
import org.mccandless.advent.intcode.Types.{ParsesIntCode, Program}

import scala.collection.mutable

object Prob23 extends ParsesIntCode with App {
  override val inputFileName = "prob23_input.txt"



  // y value of first packet sent to address 255
  def part1(program: Program): Long = {

    val inboxes: mutable.Map[Long, List[Point]] = mutable.Map.empty.withDefaultValue(Nil)

    // each machine requests its network address at startup
    val computers: Seq[Machine] = (0 to 49).map { address =>
      val m = Machine(program)
      m.run(address)
      m
    }


    def getInput(idx: Int): Point = {
      inboxes.get(idx) match {
        case Some(l :: ls) =>
          inboxes(idx) = ls
          l
        case Some(Nil) =>
          inboxes.remove(idx)
          Point(-1, -1)
        case None =>
          Point(-1, -1)
      }
    }


    var halt: Boolean = false

    while (!halt) {

      // loop over computers and run each
      computers.zipWithIndex.foreach { case (pc, address) =>
        println(s"pc $address running")
        // 3 output instructions: address, x, y
        if (!inboxes.contains(address) || inboxes(address).isEmpty) {
          println(s"no input for $address")
          val input = -1
          pc.run(input)
        }
        else {
          val input: Point = inboxes(address).head
          inboxes(address) = inboxes(address).tail
          println(s"input for $address $input")
          pc.run(input.x)
          pc.run(input.y)
        }

        val addr = pc.out
        if (addr != -1) {
          val x: Long = pc.run().output
          val y: Long = pc.run().output
          inboxes(addr) = inboxes(addr) :+ Point(x, y)
        }

        halt = addr == 255
      }

    }
    0
  }


  part1(this.input().next())
}

object Prob23Types {

}

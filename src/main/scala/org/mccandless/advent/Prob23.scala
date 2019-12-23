package org.mccandless.advent

import org.mccandless.advent.geometry.Point
import org.mccandless.advent.intcode.Machine
import org.mccandless.advent.intcode.Types.{ParsesIntCode, Program}

import scala.collection.mutable

object Prob23 extends ParsesIntCode with App {
  override val inputFileName = "prob23_input.txt"

  def getInput(inboxes: mutable.Map[Long, List[Point]], idx: Int): Option[Point] = {
    inboxes.get(idx) match {
      case Some(l :: ls) =>
        inboxes(idx) = ls
        Option(l)
      case Some(Nil) =>
        inboxes.remove(idx)
        None
      case None =>
        None
    }
  }

  // y value of first packet sent to address 255
  def part1(program: Program): Long = {

    val inboxes: mutable.Map[Long, List[Point]] = mutable.Map.empty.withDefaultValue(Nil)

    // each machine requests its network address at startup
    val computers: Seq[Machine] = (0 to 49).map { address =>
      val m = Machine(program)
      m.run(address)
      m
    }

    var halt: Boolean = false

    while (!halt) {

      // loop over computers and run each
      computers.zipWithIndex.foreach { case (pc, address) =>
        println(s"pc $address running")
        // 3 output instructions: address, x, y

        val i: Seq[Long] = getInput(inboxes, address).map { p =>
          println(s"address $address received $p")
          Seq(p.x, p.y)
        }.getOrElse(Seq(-1L))
        val o: Long = pc.run(i).output

        if (o != -1L) {

          val x = pc.run().output
          val y = pc.run().output
          println(s"sending ($x $y) to address $o")
          inboxes(o) = inboxes(o) :+ Point(x, y)

          if (o == 255) {
            halt = true
          }
        }
      }
    }
    0
  }


//  part1(this.input().next())


  def part2(program: Program): Long = {

    val inboxes: mutable.Map[Long, List[Point]] = mutable.Map.empty.withDefaultValue(Nil)

    // each machine requests its network address at startup
    val computers: Seq[Machine] = (0 to 49).map { address =>
      val m = Machine(program)
      m.run(address)
      m
    }

    var halt: Boolean = false


    while (!halt) {

      // loop over computers and run each
      computers.zipWithIndex.foreach { case (pc, address) =>
        println(s"pc $address running")
        // 3 output instructions: address, x, y

        val i: Seq[Long] = getInput(inboxes, address).map { p =>
          println(s"address $address received $p")
          Seq(p.x, p.y)
        }.getOrElse(Seq(-1L))
        val o: Long = pc.run(i).output

        if (o != -1L) {
          val x = pc.run().output
          val y = pc.run().output

          // nat receives this packet
          if (o == 255) {
            println(s"Nat received packet $x $y")
            Nat.x = x
            Nat.y = y
          }
          else {
            println(s"sending ($x $y) to address $o")
            inboxes(o) = inboxes(o) :+ Point(x, y)
          }
        }
      }


      // check if the network is idle
      if (inboxes.forall(_._2.isEmpty)) {
        println(s"network is idle, sending to address 0")
        inboxes(0) = List(Point(Nat.x, Nat.y))
      }
    }
    0
  }
  part2(this.input().next())
}



object Nat {
  var x: Long = -1
  var y: Long = -1
}

object Prob23Types {

}

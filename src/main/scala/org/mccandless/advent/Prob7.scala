package org.mccandless.advent

import org.mccandless.advent.intcode.Machine

import scala.collection.mutable

object Prob7 extends Parser[Array[Int]] with App {


  override val inputFileName = "prob7_input.txt"

  override def parse(line: String): Array[Int] = Prob5.parse(line)
  val mem: Array[Int] = input().next().clone()

  // 5 amplifiers in series


  // first amplifier input = 0



  // each amplifier needs to run a copy of program



  // input instruction used to ask for phase settings



  // then another input instruction to get amplifiers input signal, compute correct output




  // find largest output signal sent to thrusters


  //


  def thrusterSignal(initialMemory: Seq[Int], phaseSettings: (Int, Int, Int, Int, Int)): Int = {

    val ampA: Amplifier = Amplifier("A", Machine(initialMemory.toArray))
    val ampAout: Int = ampA.output(Seq(phaseSettings._1, 0))

    val ampB: Amplifier = Amplifier("B", Machine(initialMemory.toArray))
    val ambBout: Int = ampB.output(Seq(phaseSettings._2, ampAout))

    val ampC: Amplifier = Amplifier("C", Machine(initialMemory.toArray))
    val ambCout: Int = ampC.output(Seq(phaseSettings._3, ambBout))

    val ampD: Amplifier = Amplifier("D", Machine(initialMemory.toArray))
    val ambDout: Int = ampD.output(Seq(phaseSettings._4, ambCout))

    val ampE: Amplifier = Amplifier("E", Machine(initialMemory.toArray))
    ampE.output(Seq(phaseSettings._5, ambDout))
  }



  def maxThrusterSignal(program: Seq[Int]): Int = {
    Seq(0,1,2,3,4).permutations.map { perm =>
      thrusterSignal(program, (perm.head, perm(1), perm(2), perm(3), perm(4)))
    }.max
  }
//  require(maxThrusterSignal(Seq(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)) == 43210)
//  require(maxThrusterSignal(Seq(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0)) == 54321)
//  require(maxThrusterSignal(Seq(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)) == 65210)
//
//
//
//  println(maxThrusterSignal(this.mem.clone().toSeq))





  def thrusterSignalWithFeedbackLoop(program: Seq[Int], phaseSettings: (Int, Int, Int, Int, Int)): Int = {

    val ampA: Machine = Machine(program.toArray)
    val ampB: Machine = Machine(program.toArray)
    val ampC: Machine = Machine(program.toArray)
    val ampD: Machine = Machine(program.toArray)
    val ampE: Machine = Machine(program.toArray)

    val ampAInput: mutable.Buffer[Int] = mutable.Buffer(phaseSettings._1, 0)
    val ampBInput: mutable.Buffer[Int] = mutable.Buffer(phaseSettings._2)
    val ampCInput: mutable.Buffer[Int] = mutable.Buffer(phaseSettings._3)
    val ampDInput: mutable.Buffer[Int] = mutable.Buffer(phaseSettings._4)
    val ampEInput: mutable.Buffer[Int] = mutable.Buffer(phaseSettings._5)
    var halt: Boolean = false
    while (!halt) {

      val aout = ampA.run(ampAInput.toSeq).output
      ampAInput.clear()
      ampBInput += aout

      val bout = ampB.run(ampBInput.toSeq).output
      ampBInput.clear()
      ampCInput += bout

      val cout = ampC.run(ampCInput.toSeq).output
      ampCInput.clear()
      ampDInput += cout

      val dout = ampD.run(ampDInput.toSeq).output
      ampDInput.clear()
      ampEInput += dout

      val eout = ampE.run(ampEInput.toSeq)
      ampEInput.clear()
      ampAInput += eout.output

      halt = eout.halted
    }

    ampAInput.head
  }



  def maxThrusterSignalWithFeedbackLoop(program: Seq[Int]): Int = {
    // amplifiers need totally different phase settings, ints from 5-9
    Seq(5,6,7,8,9).permutations.map { perm =>
      thrusterSignalWithFeedbackLoop(program, (perm.head, perm(1), perm(2), perm(3), perm(4)))
    }.max
  }
  val ex: Int = 139629729
  require(maxThrusterSignalWithFeedbackLoop(Seq(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)) == ex)
  require(maxThrusterSignalWithFeedbackLoop(Seq(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)) == 18216)
  println(maxThrusterSignalWithFeedbackLoop(this.mem.clone().toSeq))
  // 36898044 too low
  // 61019896
}



case class Amplifier(id: String, machine: Machine) {


  def output(inputs: Seq[Int]): Int = machine.run(inputs).output
}

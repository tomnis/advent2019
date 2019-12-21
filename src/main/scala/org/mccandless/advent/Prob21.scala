package org.mccandless.advent

import org.mccandless.advent.Prob17Types.Code
import org.mccandless.advent.Prob21Types._
import org.mccandless.advent.intcode.Machine
import org.mccandless.advent.intcode.Types.{ParsesIntCode, Program}

object Prob21 extends ParsesIntCode with App {

  override val inputFileName: String = "prob21_input.txt"


  // Program the springdroid with logic that allows it to survey the hull without falling into space.
  // What amount of hull damage does it report?


   // It will begin by displaying a prompt; then,
  // input the desired instructions one per line.
  // End each line with a newline (ASCII code 10).
  // When you have finished entering your program,
  // provide the command WALK followed by a newline to instruct the springdroid to begin surveying the hull.
  def walk(spring: Machine, program: Seq[Instruction]): Option[Long] = {
    val encoded: Seq[Long] = encode(program)
    val walk: Seq[Long] = "RUN\n".toCharArray.map(_.toLong)

    spring.runUntilInput()
    println(spring.run(encoded).output.toChar)
    spring.runUntilInput()
    println(spring.run(walk).output.toChar)
    spring.runUntilInput()
    if (spring.out > 128) Option(spring.out)
    else None
  }



  def soln: Seq[Instruction] = {
    // idea: check for holes of 3, 2, 1
    // or them together

    // idea: always jump if there is ground at d
    Seq(OR(A, J), NOT(B, T), AND(T, J), AND(D, J), NOT(A, T), OR(T, J), NOT(C, T), AND(B, T), AND(A, T), AND(D, T), OR(T, J),

      // for part 2
//      NOT(D, T), AND(C, T), AND(B, T), AND(A, T), NOT(T, T), AND(T, J)
    )

      // # .. #

  }



  def tryProgramsOfLength(spring: Machine, i: Long): Option[Long] = {
    val progs: Seq[Seq[Instruction]] = getAllProgramsOfLength(i)



    progs.foreach { prog =>
      val newM: Machine = spring.snapshot()
      val res = walk(newM, prog)
      if (res.isDefined && res.get > 128) {
        return Some(res.get)
      }
    }
    None
  }



  def getAllProgramsOfLength(i: Long): Seq[Seq[Instruction]] = {
    require(i > 0 && i <= 15)


    for {
      a <- possibleInstructions
      b <- possibleInstructions
      c <- possibleInstructions
    } yield Seq(a, b, c)
  }




  // If there is ground at the given distance, the register will be true; if there is a hole, the register will be false
  def part1(program: Program): Long = {

    val spring = Machine(program)


//    tryProgramsOfLength(spring, 2).getOrElse(0)
    val prog = soln
    walk(spring, prog).getOrElse(0)

  }


  println(part1(this.input().next()))
}


object Prob21Types {


  // add newline at end
  def encode(instructions: Seq[Code]): Seq[Long] = instructions.map(_.rep).mkString("\n").toCharArray.map(_.toLong) :+ 10L



  trait Writable

  sealed trait Register extends Code
  case object A extends Register {
    override val rep: String = "A"
  }
  case object B extends Register {
    override val rep: String = "B"
  }
  case object C extends Register {
    override val rep: String = "C"
  }
  case object D extends Register {
    override val rep: String = "D"
  }
  case object E extends Register {
    override val rep: String = "E"
  }
  case object F extends Register {
    override val rep: String = "F"
  }
  case object G extends Register {
    override val rep: String = "G"
  }
  case object H extends Register {
    override val rep: String = "H"
  }
  case object I extends Register {
    override val rep: String = "I"
  }
  case object T extends Register with Writable {
    override val rep: String = "T"
  }
  case object J extends Register with Writable {
    override val rep: String = "J"
  }



  sealed trait Instruction extends Code
  case class AND(a: Register, b: Register with Writable) extends Instruction {
    override val rep: String = s"$AND ${a.rep} ${b.rep}"
  }

  case class OR(a: Register, b: Register with Writable) extends Instruction {
    override val rep: String = s"$OR ${a.rep} ${b.rep}"

  }
  case class NOT(a: Register, b: Register with Writable) extends Instruction {
    override val rep: String = s"$NOT ${a.rep} ${b.rep}"
  }


  val possibleInstructions: Seq[Instruction] = {
    for {
      i <- Seq(AND, OR, NOT)
      a <- Seq(A, B, C, D, T, J)
      b <- Seq(T, J)
    } yield i.apply(a, b)
  }



  // springdroid moves forward automatically, thinking about whether to jump
}



case class SpringDroid(program: Seq[Instruction]) {


  // Springscript programs only use Boolean values, not numbers or strings. i
  // Two registers are available: T, the temporary value register, and J,
  // the jump register. If the jump register is true at the end of the springscript program,
  // the springdroid will try to jump. Both of these registers start with the value false.
  var T: Boolean = false
  var J: Boolean = false
}
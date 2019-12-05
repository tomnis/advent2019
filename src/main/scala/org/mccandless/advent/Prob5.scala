package org.mccandless.advent

import org.mccandless.advent.intcode._
import org.mccandless.advent.intcode2.Machine


object Prob5 extends Parser[Array[Int]] with App {

  override val inputFileName = "prob5_input.txt"
  override def parse(line: String): Array[Int] = line.split(",").map(_.toInt)



  val mem: Array[Int] = input().next().clone()




  def parseInstruction(instruction: Int): Instruction = {
    val code: Int = instruction % 100

    val opCode: OpCode = OpCode(code)

    // we know its a 2 digit opcode
    val modes: List[ParameterMode] = (instruction / 100).toString.reverse.toList.map(a => ParameterMode(a.toString.toInt))

    val missingModes: List[ParameterMode] = List.fill(opCode.numParams - modes.length)(PositionMode)

    // we only support position and immmediate for now
    require(modes.forall(m => m == PositionMode || m == ImmediateMode))
    require(missingModes.forall(m => m == PositionMode || m == ImmediateMode))

    Instruction(opCode, modes.take(opCode.numParams) ++ missingModes)
  }

  require(parseInstruction(11102) == Instruction(MULT, List(ImmediateMode, ImmediateMode, ImmediateMode)))
  require(parseInstruction(1002) == Instruction(MULT, List(PositionMode, ImmediateMode, PositionMode)))





  def interpretInstruction(memory: Seq[Int], instruction: Instruction, parameters: List[Int]): MemOp = {
    require(parameters.size == instruction.opCode.numParams)
    require(parameters.size == instruction.modes.size)

    val resolvedParameters = parameters.zip(instruction.modes).take(2).map { case (parameter, mode) =>
      mode match {
        case PositionMode => memory(parameter)
        case ImmediateMode => parameter
      }
    }

    // TODO this might be wrong
    val finalResolvedParameters = if (parameters.length < 2) List(parameters.last)
    else resolvedParameters :+ parameters.last

//    println(finalResolvedParameters)
    instruction.opCode.run(finalResolvedParameters)
    // TODO output and hald


  }
  require(interpretInstruction(Seq(1002,4,3,4,33), parseInstruction(1002), List(4,3,4)) == WriteMemOp(99, 4))



  def run(memory: Array[Int]): RunResult = {
    var instructionPointer: Int = 0
    var instruction: Instruction = parseInstruction(memory(instructionPointer))

    var output: Int = -1
    while (instruction.opCode != HALT) {

      val oldInstructionPointer: Int = instructionPointer


      // interpret instruction
      val parameters: List[Int] = memory.slice(instructionPointer + 1, instructionPointer + 1 + instruction.opCode.numParams).toList

      println(s" $instructionPointer $instruction $parameters")
      instruction.opCode match {
        case ADD | MULT | INPUT | LESS_THAN | EQUALS => {
          val wOp = interpretInstruction(memory, instruction, parameters).asInstanceOf[WriteMemOp]
          println(s"$wOp")
          memory(wOp.address) = wOp.value
        }
        case JUMP_IF_TRUE | JUMP_IF_FALSE => {
          val jOp = interpretInstruction(memory, instruction, parameters).asInstanceOf[JumpOp]
          jOp.maybeNewInstructionPointer.foreach(ip => instructionPointer = ip)

        }
        case OUTPUT => {
          println(s"outputting: ${memory(parameters.head)}")
          output = memory(parameters.head)
        }
      }


      // update ip
      if (instructionPointer == oldInstructionPointer) {
        instructionPointer += (1 + instruction.opCode.numParams)
      }
      instruction = parseInstruction(memory(instructionPointer))
    }

    println("HALT\n\n")
    RunResult(output, memory.toSeq)
  }

  require(run(Array(1002,4,3,4,33)).memory ==  Seq(1002,4,3,4,99))
  // input eq 8 (position)
  require(run(Array(3,9,8,9,10,9,4,9,99,-1,8)).output == 0)
  // input lt 8 (position)
  require(run(Array(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)).output == 1)
  // input eq 8 (immediate)
  require(run(Array(3,3,1108,-1,8,3,4,3,99)).output == 0)
  // input lt 8 (immediate)
  require(run(Array(3,3,1107,-1,8,3,4,3,99)).output == 1)

  require(run(Array(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)).output == 1)

  require(run(Array(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)).output == 1)


  run(mem.clone())

  println(Machine(mem.clone()).run())
}

case class RunResult(output: Int, memory: Seq[Int])


sealed trait MemOp
case class WriteMemOp(value: Int, address: Int) extends MemOp
case class JumpOp(maybeNewInstructionPointer: Option[Int]) extends MemOp


// an opcode together with parameter modes. intentionally defined separately from any parameters
case class Instruction(opCode: OpCode, modes: List[ParameterMode])
// parameters that an instruction writes to will never be in immediate mode







sealed trait ParameterMode
// position mode causes parameter to be interpreted as a position
case object PositionMode extends ParameterMode
// immediate mode casuses parameter to be interpreted as a value
case object ImmediateMode extends ParameterMode


object ParameterMode {

  def apply(mode: Int): ParameterMode = mode match {
    case 0 => PositionMode
    case 1 => ImmediateMode
    case unknown => throw new IllegalArgumentException(s"unknown parameter mode $unknown")
  }
}


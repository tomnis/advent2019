package org.mccandless.advent

object Prob5 extends Parser[Array[Int]] with App {

  override val inputFileName = "prob5_input.txt"
  override def parse(line: String): Array[Int] = line.split(",").map(_.toInt)



  val mem: Array[Int] = input().next().clone()




  def parseInstruction(instruction: Int): Instruction = {
    val code: Int = instruction % 100
    // TODO look up number of expected args

    val opCode: OpCode = OpCode(code)

    // we know its a 2 digit opcode
    val modes: List[ParameterMode] = (instruction - code).toString.reverse.toList.drop(2).map(_.toString.toInt).map(ParameterMode.apply)

    val missingModes: List[ParameterMode] = List.fill(opCode.numParams - modes.length)(PositionMode)

    // we only support position and immmediate for now
    require(modes.forall(m => m == PositionMode || m == ImmediateMode))
    require(missingModes.forall(m => m == PositionMode || m == ImmediateMode))

    Instruction(opCode, modes.take(opCode.numParams) ++ missingModes)
  }

  require(parseInstruction(11102) == Instruction(MULT, List(ImmediateMode, ImmediateMode, ImmediateMode)))
  require(parseInstruction(1002) == Instruction(MULT, List(PositionMode, ImmediateMode, PositionMode)))





  def interpretInstruction(memory: Seq[Int], instructionPointer: Int, instruction: Instruction, parameters: List[Int]): WriteMemOp = {
    require(parameters.size == instruction.opCode.numParams)
    require(parameters.size == instruction.modes.size)

    val resolvedParameters = parameters.zip(instruction.modes).take(2).map { case (parameter, mode) =>
      mode match {
        case PositionMode => memory(parameter)
        case ImmediateMode => parameter
      }
    }

    // TODO this might be wrong
    val finalResolvedParameters = if (parameters.length < 3) List(parameters.last)
    else resolvedParameters :+ parameters.last

//    println(finalResolvedParameters)
    instruction.opCode.run(finalResolvedParameters)
    // TODO output and hald


  }
  val xyz = interpretInstruction(Seq(1002,4,3,4,33), 0, parseInstruction(1002), List(4,3,4))
  require(interpretInstruction(Seq(1002,4,3,4,33), 0, parseInstruction(1002), List(4,3,4)) == WriteMemOp(99, 4))



  def run(memory: Array[Int]): Array[Int] = {
    var instructionPointer: Int = 0
    var instruction: Instruction = parseInstruction(memory(instructionPointer))

    while (instruction.opCode != HALT) {



      // interpret instruction
      val parameters: List[Int] = memory.slice(instructionPointer + 1, instructionPointer + 1 + instruction.opCode.numParams).toList

      println(s" $instructionPointer $instruction $parameters")
      instruction.opCode match {
        case ADD | MULT | INPUT => {
          val wOp = interpretInstruction(memory, instructionPointer, instruction, parameters)
          println(s"$wOp")
          memory(wOp.address) = wOp.value
        }
        case OUTPUT => {
          println(s"outputting: ${memory(parameters.head)}")
        }
      }


      // update ip
      instructionPointer += (1 + instruction.opCode.numParams)
      instruction = parseInstruction(memory(instructionPointer))
    }

    memory
  }

  require(run(Array(1002,4,3,4,33)).toList == Array(1002,4,3,4,99).toList)


  run(mem)
}



case class WriteMemOp(value: Int, address: Int)


case class Instruction(opCode: OpCode, modes: List[ParameterMode])

// parameters that an instruction writes to will never be in immediate mode


sealed trait OpCode {
  val numParams: Int

  def run(resolvedParameters: Seq[Int]): WriteMemOp

}
case object ADD extends OpCode {
  override val numParams: Int = 3


  override def run(resolvedParameters: Seq[Int]): WriteMemOp = WriteMemOp(
    resolvedParameters(0) + resolvedParameters(1),
    resolvedParameters(2)
  )
}

case object MULT extends OpCode {
  override val numParams: Int = 3

  override def run(resolvedParameters: Seq[Int]): WriteMemOp = WriteMemOp(
    resolvedParameters(0) * resolvedParameters(1),
    resolvedParameters(2)
  )
}

case object INPUT extends OpCode {
  override val numParams: Int = 1

  override def run(resolvedParameters: Seq[Int]): WriteMemOp = WriteMemOp(
    1, resolvedParameters.head
  )
}

case object OUTPUT extends OpCode {
  override val numParams: Int = 1
  override def run(resolvedParameters: Seq[Int]): WriteMemOp = ???
}




// if param is nonzero, sets ip to second parameter
case object JUMP_IF_TRUE extends OpCode {
  override val numParams: Int = 2
  override def run(resolvedParameters: Seq[Int]): WriteMemOp = ???
}


// if param is zero, sets ip to second parameter
case object JUMP_IF_FALSE extends OpCode {
  override val numParams: Int = 2
  override def run(resolvedParameters: Seq[Int]): WriteMemOp = ???
}



case object LESS_THAN extends OpCode {
  override val numParams: Int = 3
  override def run(resolvedParameters: Seq[Int]): WriteMemOp = ???
}



case object HALT extends OpCode {
  override val numParams: Int = 0
  override def run(resolvedParameters: Seq[Int]): WriteMemOp = ???
}

object OpCode {
  def apply(opCode: Int): OpCode = {
    opCode match {
      case 1 => ADD
      case 2 => MULT
      case 3 => INPUT
      case 4 => OUTPUT
      case 99 => HALT
      case other => throw new IllegalArgumentException(s"unknown opcode $other")
    }
  }
}





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


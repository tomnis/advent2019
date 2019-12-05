package org.mccandless.advent.intcode


import TypeAliases._
import org.mccandless.advent.intcode.MULT.run
import org.mccandless.advent.{JumpOp, MemOp, ParameterMode, WriteMemOp}


// TODO fill in
//trait TernaryOp extends OpCode {
//
//}


sealed trait OpCode {
  type Arity[_] <: Product
  type Params = Arity[Int]

  val numParams: Int

//  def resolve(memory: Seq[Int], modes: Arity[ParameterMode], parameters: List[Int]): Arity[Int]

  def run(resolvedParameters: List[Int]): MemOp

  // todo rename execute
  def run(resolvedParameters: Params): MemOp
}





case object ADD extends OpCode {
  override type Arity[_] = Ternary[_]
//  override type Input = (Int, Int, Int)
  override val numParams: Int = 3


  override def run(resolvedParameters: Params): MemOp = {
    val (a: Int, b: Int, addr: Int) = resolvedParameters
    WriteMemOp(a + b, addr)
  }

  override def run(resolvedParameters: List[Int]): MemOp = run(resolvedParameters.head, resolvedParameters(1), resolvedParameters(2))
}


case object MULT extends OpCode {
  override type Arity[_] = Ternary[_]
  override val numParams: Int = 3

  override def run(resolvedParameters: Params): MemOp = {
    val (a: Int, b: Int, addr: Int) = resolvedParameters
    WriteMemOp(a * b, addr)
  }
  override def run(resolvedParameters: List[Int]): MemOp = run(resolvedParameters.head, resolvedParameters(1), resolvedParameters(2))
}


case object INPUT extends OpCode {
  override type Arity[_] = Unary[_]
  override val numParams: Int = 1

  // TODO ask the oracle
//  override def run(resolvedParameters: Arity[Int]): MemOp = WriteMemOp( 5, resolvedParameters._1)
  override def run(resolvedParameters: List[Int]): MemOp = run(new Tuple1[Int](resolvedParameters.head))

  override def run(resolvedParameters: Params): MemOp = WriteMemOp(5, resolvedParameters._1.toString.toInt)
}




case object OUTPUT extends OpCode {
  override type Arity[_] = Unary[_]
  override val numParams: Int = 1
  override def run(resolvedParameters: Params): MemOp = ???

  override def run(resolvedParameters: List[Int]): MemOp = ???
}



// if param is nonzero, sets ip to second parameter
case object JUMP_IF_TRUE extends OpCode {
  override type Arity[_] = Binary[_]
  override val numParams: Int = 2
  override def run(resolvedParameters: Params): JumpOp = {
    val (a: Int, ip: Int) = resolvedParameters
    JumpOp(if (a != 0) Option(ip) else None)
  }

  override def run(resolvedParameters: List[Int]): MemOp = run(resolvedParameters.head, resolvedParameters(1))
}


// if param is zero, sets ip to second parameter
case object JUMP_IF_FALSE extends OpCode {
  override type Arity[_] = Binary[_]
  override val numParams: Int = 2
  override def run(resolvedParameters: Params): JumpOp = {
    val (a: Int, ip: Int) = resolvedParameters
    JumpOp(if (a == 0) Option(ip) else None)
  }

  override def run(resolvedParameters: List[Int]): MemOp = run(resolvedParameters.head, resolvedParameters(1))
}


// if first parameter is less than second parameter, stores 1 in 3rd parameter
case object LESS_THAN extends OpCode {
  override type Arity[_] = Ternary[_]
  override val numParams: Int = 3
  override def run(resolvedParameters: Params): WriteMemOp = {
    val (a: Int, b: Int, addr: Int) = resolvedParameters
    WriteMemOp(if (a < b) 1 else 0, addr)
  }
  override def run(resolvedParameters: List[Int]): MemOp = run(resolvedParameters.head, resolvedParameters(1), resolvedParameters(2))
}



// if first parameter is equal to second parameter, stores 1 in 3rd parameter
case object EQUALS extends OpCode {
  override type Arity[_] = Ternary[_]
  override val numParams: Int = 3
  override def run(resolvedParameters: Params): WriteMemOp = {
    val (a: Int, b, addr: Int) = resolvedParameters
    WriteMemOp(if (a == b) 1 else 0, addr)
  }
  override def run(resolvedParameters: List[Int]): MemOp = run(resolvedParameters.head, resolvedParameters(1), resolvedParameters(2))
}


case object HALT extends OpCode {
  override type Arity[_] = Nullary[_]
  override val numParams: Int = 0
  override def run(resolvedParameters: Params): WriteMemOp = ???
  override def run(resolvedParameters: List[Int]): MemOp = ???
}


object OpCode {

  def apply(opCode: Int): OpCode = {
    opCode match {
      case 1 => ADD
      case 2 => MULT
      case 3 => INPUT
      case 4 => OUTPUT
      case 5 => JUMP_IF_TRUE
      case 6 => JUMP_IF_FALSE
      case 7 => LESS_THAN
      case 8 => EQUALS
      case 99 => HALT
      case other => throw new IllegalArgumentException(s"unknown opcode $other")
    }
  }
}

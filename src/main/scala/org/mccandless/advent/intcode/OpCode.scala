package org.mccandless.advent.intcode


// goal: dont pass the memory around
sealed trait Op {

  val numInputs: Int
  val numOutputs: Int
  val modes: Seq[Mode]

  def numParams: Int = numInputs + numOutputs

  // assumes input params have already been resolved
  def run(params: Seq[Long]): Effect
}



trait Nullary extends Op {
  final override val numInputs: Int = 0
  final override val numOutputs: Int = 1
}

trait Unary extends Op {
  final override val numInputs: Int = 1
  final override val numOutputs: Int = 1
}

// a binary operation with 2 inputs and a single output
trait Binary extends Op {
  final override val numInputs: Int = 2
  final override val numOutputs: Int = 1
}



case class ADD(modes: Seq[Mode]) extends Op with Binary {
  override def run(params: Seq[Long]): Effect = Write(params.head + params(1), params(2))
}

case class MULT(modes: Seq[Mode]) extends Op with Binary {
  override def run(params: Seq[Long]): Effect = Write(params.head * params(1), params(2))
}

case class INPUT(modes: Seq[Mode]) extends Op {
  override val numInputs: Int = 0
  override val numOutputs: Int = 1
  override def run(params: Seq[Long]): Effect = {
    println(s"input run: params=$params")
    Write(Oracle.input(), params.head)
  }
}

case class OUTPUT(modes: Seq[Mode]) extends Op {
  override val numInputs: Int = 1
  override val numOutputs: Int = 0
  override def run(params: Seq[Long]): Effect = Print(params.head)
}

case class JUMP_IF_TRUE(modes: Seq[Mode]) extends Op {
  override val numInputs: Int = 2
  override val numOutputs: Int = 0
  override def run(params: Seq[Long]): Effect = if (params.head != 0) Jump(params(1)) else Pure
}

case class JUMP_IF_FALSE(modes: Seq[Mode]) extends Op {
  override val numInputs: Int = 2
  override val numOutputs: Int = 0
  override def run(params: Seq[Long]): Effect = if (params.head == 0) Jump(params(1)) else Pure
}

case class LESS_THAN(modes: Seq[Mode]) extends Op with Binary {
  override def run(params: Seq[Long]): Effect = Write(if (params.head < params(1)) 1 else 0, params(2))
}

case class EQUALS(modes: Seq[Mode]) extends Op with Binary {
  override def run(params: Seq[Long]): Effect = Write(if (params.head == params(1)) 1 else 0, params(2))
}


case class REL_BASE(modes: Seq[Mode]) extends Op {
  override val numInputs: Int = 1
  override val numOutputs: Int = 0

  override def run(params: Seq[Long]): Effect = IncRelBase(params.head)
}

case object HALT extends Op {
  override val numInputs: Int = 0
  override val numOutputs: Int = 0
  override val modes: Seq[Mode] = Seq.empty

  // TODO separate halt effect?
  override def run(params: Seq[Long]): Effect = Pure
}



object Op {

  def apply(op: Long): Op = {
    // 2 digit opcode
    val opCode: Long = op % 100
    val modes: Seq[Mode] = Mode.parse(op / 100)

    opCode match {
      case 1 => ADD(modes take 3)
      case 2 => MULT(modes take 3)
      case 3 => INPUT(modes take 1)
      case 4 => OUTPUT(modes take 1)
      case 5 => JUMP_IF_TRUE(modes take 2)
      case 6 => JUMP_IF_FALSE(modes take 2)
      case 7 => LESS_THAN(modes take 3)
      case 8 => EQUALS(modes take 3)
      case 9 => REL_BASE(modes take 1)
      case 99 => HALT
      case other => throw new IllegalArgumentException(s"unknown opcode $other")
    }
  }
}

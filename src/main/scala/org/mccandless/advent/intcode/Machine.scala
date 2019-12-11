package org.mccandless.advent.intcode

import scala.annotation.tailrec
import scala.collection.mutable

case class Machine(program: Seq[Long]) {

  var out: Long = 0
  var ip: Long = 0
  var relBase: Long = 0
  val mem: mutable.Map[Long, Long] = mutable.Map.empty.withDefaultValue(0L) ++ program.zipWithIndex.map(a => (a._2.toLong, a._1)).toMap

  @tailrec final def run(inputs: Seq[Long] = Seq.empty): MachineState = {
    val op: Op = Op(mem(ip))
    op match {
      case HALT =>
        halt
      // we dont have any input for now, pause
      case INPUT(_, _) if inputs.isEmpty =>
        pause
      case input @ INPUT(_, _) =>
        process(input.copy(maybeIn = inputs.headOption))
        run(inputs.tail)
      case OUTPUT(_) =>
        process(op)
        pause
      case _ =>
        process(op)
        run(inputs)
    }
  }


  def process(op: Op): Effect = {
    val params: Seq[Long] = (ip + 1).until(ip + 1 + op.numParams).map(mem(_))
    println(s"$op ${params.toList}")
    val resolved: Seq[Long] = resolve(op, params)
    val effect: Effect = op.run(resolved)
    println(s"effect: $effect")
    mutate(effect, 1 + op.numParams)
    effect
  }


  def pause: MachineState = Paused(out)

  def halt: MachineState = {
    println("HALT\n\n")
    Halted(out)
  }

  def resolve(op: Op, params: Seq[Long]): Seq[Long] = {
    val resolvedInputs = params.take(op.numInputs).zip(op.modes).map {
      case (addr, Position) => mem(addr)
      case (addr, Relative) => mem(addr + relBase)
      case (v, Immediate) => v
      case error => throw new RuntimeException(s"error: $error")
    }

    // outputs cannot be in immediate mode
    val resolvedOutputs = params.slice(op.numInputs, op.numInputs + op.numOutputs).zip(op.modes.drop(op.numInputs)).map {
      case (addr, Position) => addr
      case (addr, Relative) => addr + relBase
      case error => throw new RuntimeException(s"error: $error")
    }

    resolvedInputs ++ resolvedOutputs
  }


  // this should be the only place we mutate values
  def mutate(effect: Effect, ipOffset: Int): Unit = {
    def incIp(): Unit = ip += ipOffset
    effect match {
      case Pure =>
        incIp()
      case Store(value, addr) =>
        mem(addr) = value
        incIp()
      case Jump(newIp) =>
        ip = newIp.toInt
      case Print(value) =>
        out = value
        incIp()
      case IncRelBase(diff) =>
        relBase += diff
        incIp()
    }
  }
}




sealed trait MachineState {
  val output: Long
}
case class Paused(output: Long) extends MachineState
case class Halted(output: Long) extends MachineState

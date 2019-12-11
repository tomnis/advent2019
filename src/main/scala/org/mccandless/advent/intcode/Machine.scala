package org.mccandless.advent.intcode

import scala.annotation.tailrec
import scala.collection.mutable

case class Machine(memory: Array[Long]) {

  var out: Long = 0
  var ip: Int = 0
  var relBase: Long = 0
//  val mem: mutable.Map[Long, Long] = mutable.Map.empty.withDefaultValue(0L) ++ memory.zipWithIndex.map(a => (a._1, a._2.toLong)).toMap

  @tailrec final def run(inputs: Seq[Long] = Seq.empty): MachineState = {
    val op: Op = Op(memory(ip))
    op match {
      case HALT =>
        halt
      // we dont have any input for now, pause
      case INPUT(_, _) if inputs.isEmpty =>
        pause
      case INPUT(_, _) =>
        process(op, inputs.headOption)
        run(inputs.tail)
      case OUTPUT(_) =>
        process(op)
        pause
      case _ =>
        process(op)
        run(inputs)
    }
  }


  def process(op: Op, maybeInput: Option[Long] = None): Effect = {
    val finalOp: Op = op match {
      case in @ INPUT(_, _) => in.copy(maybeIn = maybeInput)
      case other => other
    }
    val params: Seq[Long] = memory.slice(ip + 1, ip + 1 + finalOp.numParams)
    println(s"$finalOp ${params.toList}")
    val resolved: Seq[Long] = resolve(finalOp, params)
    val effect: Effect = finalOp.run(resolved)
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
       case (addr, Position) => memory(addr.toInt)
       case (addr, Relative) => memory((addr.toInt + relBase).toInt)
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




  def mutate(effect: Effect, ipOffset: Int): Unit = {
    def incIp(): Unit = ip += ipOffset
    effect match {
      case Pure =>
        incIp()
      case Write(value, addr) =>
        memory(addr.toInt) = value
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

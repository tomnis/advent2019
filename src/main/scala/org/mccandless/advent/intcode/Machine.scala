package org.mccandless.advent.intcode

import scala.annotation.tailrec

case class Machine(memory: Array[Long]) {

  var out: Long = 0
  var ip: Int = 0
  var relBase: Long = 0

  @tailrec final def run(inputs: Seq[Long] = Seq.empty): EndState = {
    val op: Op = Op(memory(ip))

    op match {
      case HALT => {
        println("HALT\n\n")
        EndState(out, halted=true, memory.toSeq)
      }
      case input @ INPUT(_) => {
        // if we don't have any inputs for now, return without modifying ip
        if (inputs.isEmpty) {
          EndState(out, halted = false, memory.toSeq)
        }
        else {
          val params: Array[Long] = memory.slice(ip + 1, ip + 1 + input.numParams)
          println(s"$input params=${params.toList} inputs=${inputs.toList}")
          val resolved: Seq[Long] = resolve(input, params)
          val effect: Effect = input.run(resolved).asInstanceOf[Write].copy(value = inputs.head)
          println(s"effect: $effect")
          mutate(effect)

          ip += (1 + input.numParams)
          if (inputs.nonEmpty) {
            run(inputs.tail)
          }
          else {
            EndState(out, halted=false, memory.toSeq)
          }
        }
      }
      case other => {
        val oldIp: Int = ip
        val params: Array[Long] = memory.slice(ip + 1, ip + 1 + other.numParams)
        println(s"$other ${params.toList}")
        val resolved: Seq[Long] = resolve(other, params)
        val effect: Effect = op.run(resolved)
        println(s"effect: $effect")
        mutate(effect)
        // if we haven't changed ip, increment ip
        if (oldIp == ip) {
          ip += (1 + other.numParams)
        }
        run(inputs)
      }
    }
  }



   def resolve(op: Op, params: Seq[Long]): Seq[Long] = {
     val resolvedInputs = params.take(op.numInputs).zip(op.modes).map {
       case (addr, Position) => {
        println(s"resolve position: addr = $addr, value=${memory(addr.toInt)}")
        memory(addr.toInt)
      }
      case (addr, Relative) => {
        println(s"resolve relative: addr = $addr, relbase:$relBase, value: ${memory((addr + relBase).toInt)}")
        memory((addr + relBase).toInt)
      }
      case (v, Immediate) => {
        println(s"resolve immediate: v = $v")
        v
      }
      case error => throw new RuntimeException(s"error: $error")
     }


     // outputs cannot be in immediate mode
     val resolvedOutputs = params.drop(op.numInputs).take(op.numOutputs).zip(op.modes.drop(op.numInputs)).map {
       case (addr, Position) => {
        println(s"resolve position: addr = $addr, value=${memory(addr.toInt)}")
        addr
      }
      case (addr, Relative) => {
        println(s"resolve relative: addr = $addr, relbase:$relBase, value: ${memory((addr + relBase).toInt)}")
        addr + relBase
      }
      case error => throw new RuntimeException(s"error: $error")
     }

     resolvedInputs ++ resolvedOutputs
   }




  def mutate(e: Effect): Unit = e match {
    case Pure =>
    case Write(value, addr) => memory(addr.toInt) = value
    case Jump(newIp) => ip = newIp.toInt
    case Print(value) => out = value
    case IncRelBase(diff) => relBase += diff
  }
}




case class EndState(output: Long, halted: Boolean, memory: Seq[Long]) {
}

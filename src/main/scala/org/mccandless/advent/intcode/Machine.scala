package org.mccandless.advent.intcode

import scala.annotation.tailrec

case class Machine(memory: Array[Int]) {

  var out: Int = 0
  var ip: Int = 0

  @tailrec final def run(inputs: Seq[Int] = Seq.empty): EndState = {
    val op = Op(memory(ip))

    op match {
      case HALT => {
        println("HALT\n\n")
        EndState(out, memory.toSeq)
      }
      case input @ INPUT(_) => {
        // if we don't have any inputs for now, return without modifying ip
        if (inputs.isEmpty) {
          EndState(out, memory.toSeq, halted = false)
        }
        else {
          val params: Array[Int] = memory.slice(ip + 1, ip + 1 + input.numParams)
          println(s"$input params=${params.toList} inputs=${inputs.toList}")
          val resolved: Seq[Int] = resolve(input, params)
          val effect: Effect = input.run(resolved).asInstanceOf[Write].copy(value = inputs.head)
          mutate(effect)

          ip += (1 + input.numParams)
          if (inputs.nonEmpty) {
            run(inputs.tail)
          }
          else {
            EndState(out, memory.toSeq, halted = false)
          }
        }
      }
      case other => {
        val oldIp: Int = ip
        val params: Array[Int] = memory.slice(ip + 1, ip + 1 + other.numParams)
        println(s"$other ${params.toList}")
        val resolved: Seq[Int] = resolve(other, params)
        val effect: Effect = op.run(resolved)
        mutate(effect)
        // if we haven't changed ip, increment ip
        if (oldIp == ip) {
          ip += (1 + other.numParams)
        }
        run(inputs)
      }
    }
  }


   def resolve(op: Op, params: Seq[Int]): Seq[Int] = {
    val x = params.take(op.numInputs).zip(op.modes).map {
      case (addr, Position) => memory(addr)
      case (v, Immediate) => v
    } ++ params.drop(op.numInputs)
    require(x.length == op.numParams)
     x
   }


  def mutate(e: Effect): Unit = e match {
    case Pure =>
    case Write(value, addr) => memory(addr) = value
    case Jump(newIp) => ip = newIp
    case Print(addr) => {
      out = memory(addr)
      println(s"debug: ${memory(addr)}")
    }
  }
}


case class EndState(output: Int, memory: Seq[Int], halted: Boolean = true)

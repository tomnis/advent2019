package org.mccandless.advent.intcode2

import scala.annotation.tailrec


trait Result
case class Output(value: Int) extends Result

case class Machine(memory: Array[Int]) {

  var out: Int = 0
  var ip: Int = 0

  @tailrec final def run(): Result = {
    val op = Op(memory(ip))

    op match {
      case HALT => Output(out)
      case other => {
        println(other)
        val params = memory.slice(ip + 1, ip + 1 + other.numParams)
        val resolved = resolve(other, params)
        val effect = op.run(resolved)
        processEffect(effect)
        run()
      }
    }
  }




   def resolve(op: Op, params: Seq[Int]): Seq[Int] = {
    params.take(op.numInputs).zip(op.modes).map {
      case (addr, Position) => memory(addr)
      case (v, Immediate) => v
    } ++ params.drop(op.numInputs)
  }

  def processEffect(e: Effect): Unit = e match {
    case Pure =>
    case Write(value, addr) => memory(addr) = value
    case Jump(newIp) => ip = newIp
    case Debug(addr) => print(memory(addr))
  }
}



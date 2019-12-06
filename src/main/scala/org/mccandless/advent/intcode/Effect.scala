package org.mccandless.advent.intcode

sealed trait Effect

case object Pure extends Effect
case class Write(value: Int, addr: Int) extends Effect
case class Jump(ip: Int) extends Effect
case class Print(addr: Int) extends Effect

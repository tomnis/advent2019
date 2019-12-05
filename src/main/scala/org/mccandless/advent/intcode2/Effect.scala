package org.mccandless.advent.intcode2

sealed trait Effect

case object Pure extends Effect
case class Write(value: Int, addr: Int) extends Effect
case class Jump(ip: Int) extends Effect
case class Debug(addr: Int) extends Effect

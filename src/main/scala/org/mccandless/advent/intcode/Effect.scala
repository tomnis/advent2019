package org.mccandless.advent.intcode

sealed trait Effect

case object Pure extends Effect
case class Write(value: Long, addr: Long) extends Effect
case class Jump(ip: Long) extends Effect
case class Print(addr: Long) extends Effect
// adjusts relative base
case class IncRelBase(diff: Long) extends Effect

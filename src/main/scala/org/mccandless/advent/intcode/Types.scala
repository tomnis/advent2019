package org.mccandless.advent.intcode

import org.mccandless.advent.util.Parser

object Types {

  type Program = Seq[Long]

  trait ParsesIntCode extends Parser[Program] {
    override def parse(line: String): Program = line.split(",").toSeq.map(_.toLong)
  }
}

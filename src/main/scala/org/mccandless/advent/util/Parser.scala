package org.mccandless.advent.util

import scala.io.{BufferedSource, Source}

/**
 *
 * Created by tdm on 2019-12-02.
 */
trait Parser[I] {
  val inputFileName: String

  def input(fileName: String = this.inputFileName): Iterator[I] = {
    val source: BufferedSource = Source.fromResource(fileName)
    val res: Iterator[I] = source.getLines.map(parse).toList.iterator
    source.close()
    res
  }

  def parse(line: String): I
}

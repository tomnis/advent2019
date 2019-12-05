package org.mccandless.advent

import scala.io.{BufferedSource, Source}

/**
 *
 * Created by tdm on 2019-12-02.
 */
trait Parser[I] {
  val inputFileName: String

  def input(): Iterator[I] = {
    val source: BufferedSource = Source.fromResource(inputFileName)
    val res: Iterator[I] = source.getLines.map(parse).toList.iterator
    source.close()
    res
  }

  def parse(line: String): I
}
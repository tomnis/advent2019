package org.mccandless.advent

import org.scalatest._

class HelloSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    "hello" shouldEqual "hello"
  }
}

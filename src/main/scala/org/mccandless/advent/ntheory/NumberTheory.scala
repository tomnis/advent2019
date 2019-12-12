package org.mccandless.advent.ntheory

object NumberTheory {

  def gcd(a: Long, b: Long): Long = if (a == 0) b else gcd(b % a, a)
  require(gcd(366, 60) == 6)
  require(gcd(60, 366) == 6)


  def lcm(x: Long, y: Long): Long = {
    x / gcd(x, y) * y
  }


  def lcm(x: Long, y: Long, z: Long): Long = {
    lcm(lcm(x,y), z)
  }
}

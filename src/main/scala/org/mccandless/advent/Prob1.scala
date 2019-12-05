package org.mccandless.advent

import scala.annotation.tailrec

/**
 *
 * Created by tdm on 2019-12-01.
 */
object Prob1 extends Parser[Long] with App {

  override val inputFileName: String = "prob1_input.txt"

  override def parse(line: String): Long = line.toLong

  // to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2.
  def fuelRequired(mass: Long): Long = {
    @tailrec def fuelRequiredForMass(mass: Long, acc: Long = 0): Long = {
      val fuel: Long = mass / 3 - 2
      if (fuel <= 0) acc
      else fuelRequiredForMass(fuel, acc + fuel)
    }

    fuelRequiredForMass(mass, 0)
  }
  require(fuelRequired(1969L) == 966L)
  require(fuelRequired(100756L) == 50346L)

  println(input().map(fuelRequired).sum)
}

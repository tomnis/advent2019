package org.mccandless.advent

import Prob14Types._
import org.mccandless.advent.util.Parser

import scala.collection.mutable

/**
 * Created by tomas.mccandless on 12/13/19.
 */
object Prob14 extends Parser[Reaction] with App {
  override val inputFileName = "prob14_input.txt"

  override def parse(line: String) = {
    val ios: Array[String] = line.split(" => ")
    val inputs: Seq[RComp] = ios.head.split(", ").map(RComp(_)).toSeq
    val outputs: RComp = RComp(ios(1))
    Reaction(inputs, outputs)
  }

  require(parse("44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL") == Reaction(Seq(
    RComp(44, "XJWVT"),
    RComp(5, "KHKGT"),
    RComp(1, "QDVJ"),
    RComp(29, "NZVS"),
    RComp(9, "GPVTF"),
    RComp(48, "HKGWZ")
  ),
    RComp(1, "FUEL")
  ))


  // a meterial is basic if we only need ore
  def oreRequiredForFuel(reactions: Seq[Reaction], amount: Long): Long = {
    // map materials to (outputcount, Seq[Inputs]
    // map 'FUEL': (1, ((7, 'A'), (1, 'E')))
    val knownReactions: Map[String, ReactInfo] = reactions.map( r =>
      r.output.material -> ReactInfo(r.output.count, r.inputs)
    ).toMap

    val targets: mutable.Map[String, Long] = mutable.Map.empty.withDefaultValue(0L) ++ Map("FUEL" -> amount)
    val remainders: mutable.Map[String, Long] = mutable.Map.empty.withDefaultValue(0L)
    var ore: Long = 0



    while (targets.nonEmpty) {
      // get next target
      val (material, outputCount) = targets.head
//      println(s"processing target: ${targets.head}")
      // get the output count and inputs required for this target
      val info: ReactInfo = knownReactions(material)
      val targetOutput = info.outputCount

      // figure out how many times we need to run the reaction
      var qnt = if (outputCount < 0) -1 else outputCount / info.outputCount
      val rem = if (outputCount < 0) targetOutput - (scala.math.abs(outputCount) % targetOutput) else outputCount % targetOutput
      targets -= material

      if (rem != 0) {
        remainders(material) = (targetOutput -  rem)
        qnt += 1
      }

      info.inputs.foreach { input: RComp =>
        val cnt = input.count
        val mat = input.material
        if (input.material == "ORE") {
          ore += (qnt * cnt) - remainders(mat)
        }
        else {
          targets(input.material) += (qnt * cnt - remainders(mat))
          remainders(input.material) = 0
        }
      }
    }


    ore
  }


  // min ore required for 1 fuel
  def part1(reactions: Seq[Reaction]): Long = {
    println("trying part 1")

    // map outputs to inputs
    oreRequiredForFuel(reactions, 1)
  }

  println(part1(this.input().toSeq))



  def part2(reactions: Seq[Reaction]): Long = {

    // greatest amount of fuel we can produce with 1trillion ore
    val tril: Long = 1000000000000L

    var lowFuel: Long = 1
    var highFuel: Long = 1
    while (oreRequiredForFuel(reactions, highFuel) < tril) {
      highFuel = highFuel * 2
    }


    var break = false
    while (lowFuel < highFuel - 1 && !break) {
      val mid = lowFuel + ((highFuel - lowFuel) / 2)
      val ore = oreRequiredForFuel(reactions, mid)

      if (ore < tril) {
        lowFuel = mid
      }
      else if (ore > tril) {
        highFuel = mid
      }
      else {
        // found it
        break = true
      }
    }

    println(s"$lowFuel $highFuel")
    println(oreRequiredForFuel(reactions, lowFuel))
    println(oreRequiredForFuel(reactions, highFuel))



    0
  }

  println(part2(this.input().toSeq))
}








object Prob14Types {

  case class ReactInfo(outputCount: Long, inputs: Seq[RComp])

  case class Reaction(inputs: Seq[RComp], output: RComp)

  case class RComp(count: Long, material: String)

  // 171 ORE
  object RComp {
    def apply(r: String): RComp = {
      val args = r.split(" ")
      val count = args.head.toLong
      val material = args(1)
      RComp(count, material)
    }
  }
}

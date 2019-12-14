package org.mccandless.advent

import Prob14Types._
import org.mccandless.advent.util.Parser

import scala.collection.mutable

/**
 * Created by tomas.mccandless on 12/13/19.
 */
object Prob14 extends Parser[Reaction] with App {
  override val inputFileName = "prob14_input_small4.txt"

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
  def isBasic(reactions: Seq[Reaction], material: String): Boolean = {
    val out = reactions.filter(_.output.material == material)
    require(out.size == 1)
    out.head.inputs.forall(_.material == "ORE")
  }

  def newOnes(m: Map[RComp, Seq[RComp]], nonBasic: RComp): Seq[RComp] = {

    println(s"adding: new ones ${m}")
    val sameMaterial = m.view.filterKeys(_.material == nonBasic.material).toMap
    require(sameMaterial.size == 1)

    val sme = sameMaterial.head
    // should be sme._2 duplicated a certain number of times?
    val count = if (nonBasic.count % sme._1.count == 0) nonBasic.count.toInt / sme._1.count.toInt
    else scala.math.ceil(nonBasic.count.toDouble / sme._1.count.toInt).toInt

    Seq.fill(count)(sme._2).flatten
  }

  def rewrite(reactions: Seq[Reaction]): Long = {
    val m: mutable.Map[RComp, Seq[RComp]] = mutable.Map.empty ++ reactions.map(r => r.output -> r.inputs).toMap
    val containsFuelReaction = m.filter(_._1.material == "FUEL")
    require(containsFuelReaction.size == 1)
    val fuelReaction= containsFuelReaction.head

    var fuelInputs: mutable.Buffer[RComp] = mutable.Buffer.empty ++ fuelReaction._2
    println(s"current inputs: $fuelInputs")

    while (!fuelInputs.forall(input => isBasic(reactions, input.material))) {
      // get first material that is not basic, and get its inputs
      val nonbasic: RComp = fuelInputs.find(input => !isBasic(reactions, input.material)).get
      val idx = fuelInputs.indexOf(nonbasic)
      fuelInputs.remove(idx)

      println(s"found nonbasic material: $nonbasic")
      fuelInputs ++= newOnes(m.toMap, nonbasic)
    }
    // reduce
    val mutableinputs: mutable.Map[String, Long] = mutable.Map.empty.withDefaultValue(0L)
    fuelInputs.foreach { i =>
      mutableinputs(i.material) += i.count
    }

    println(s"mutable inputs $mutableinputs")
    mutableinputs.map { case (material, count) =>
        // get the ore required for making count of material (should be basic)
      oreRequiredFor(material, count, reactions)
    }.sum
  }



  def oreRequiredFor(material: String, count: Long, reactions: Seq[Reaction]): Long = {
    require(isBasic(reactions, material))
    val r: Seq[Reaction] = reactions.filter(_.output.material == material)
    require(r.size == 1)
    val s = r.head

    val x = scala.math.ceil(count * 1.0 / s.output.count)
    x.toLong * s.inputs.head.count
  }





  // min ore required for 1 fuel
  def part1(reactions: Seq[Reaction]): Long = {
    println("trying part 1")

    // map outputs to inputs

    rewrite(reactions)
  }


  println(part1(this.input().toSeq))
}








object Prob14Types {


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

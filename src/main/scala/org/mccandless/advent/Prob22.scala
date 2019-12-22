package org.mccandless.advent

import org.mccandless.advent.Prob22Types.{Card, Cut, CutNegative, DealIntoStack, DealWithIncrement, Deck, ShuffleOp}
import org.mccandless.advent.util.Parser

import scala.collection.mutable

object Prob22 extends Parser[ShuffleOp] with App {

  override val inputFileName: String = "prob22_input.txt"

  override def parse(line: String): ShuffleOp = ShuffleOp(line)

  /**
   * card shuffle
   *
   * 10007 cards
   *
   *
   */


  def dealIntoStack(cards: Seq[Card]): Seq[Card] = cards.reverse



  // To cut N cards, take the top N cards off the top of the deck and move them as a single unit to the bottom of the deck
  def cutN(deck: Deck, n: Int): Deck = {
    require(n > 0)
    deck.drop(n) ++ deck.take(n)
  }


  def cutNegativeN(deck: Deck, n: Int): Deck = {
    require(n > 0)
    deck.takeRight(n) ++ deck.dropRight(n)
  }
  println(cutNegativeN((0 to 9).map(Card(_)), 3))




  def dealWithIncrement(deck: Deck, n: Int): Deck = {
    var oldDeck = deck
    val newDeck: mutable.Buffer[Card] = mutable.Buffer.fill(deck.length)(Card(-1))



    var newIdx = 0
    while (oldDeck.nonEmpty) {
      newDeck(newIdx) = oldDeck.head
      oldDeck = oldDeck.tail

      newIdx += n
      if (newIdx >= deck.length - 1) {
        newIdx = newIdx % deck.length
      }
    }

    newDeck.toSeq
  }

  println(dealWithIncrement((0 to 9).map(Card(_)), 3))




  def part1(ops: Seq[ShuffleOp]): Int = {
    var deck: Seq[Card] = (0 to 10006).map(i => Card(i))

    ops foreach { op =>
      val newDeck = op match {
        case Cut(n) => cutN(deck, n)
        case CutNegative(n) => cutNegativeN(deck, n)
        case DealIntoStack => dealIntoStack(deck)
        case DealWithIncrement(n) => dealWithIncrement(deck, n)
      }

      deck = newDeck
    }

    deck.zipWithIndex.filter(_._1.number == 2019).head._2
  }


//  println(part1(this.input().toSeq))


  def part2(ops: Seq[ShuffleOp]): Long = {


    var deck: Seq[Card] = (0L to 119315717514047L).map(i => Card(i))

    ops foreach { op =>
      val newDeck = op match {
        case Cut(n) => cutN(deck, n)
        case CutNegative(n) => cutNegativeN(deck, n)
        case DealIntoStack => dealIntoStack(deck)
        case DealWithIncrement(n) => dealWithIncrement(deck, n)
      }

      deck = newDeck

      println(s"at 2020: ${deck(2020)}")
    }

    0
  }

  part2(this.input().toSeq)
}


object Prob22Types {
  type Deck = Seq[Card]

  case class Card(number: Long)




  val incrementR = "deal with increment (\\d+)".r
  val dealStack = "deal into new stack".r
  val cut = "cut (\\d+)".r
  val cutNegative = "cut -(\\d+)".r


  sealed trait ShuffleOp
  case class Cut(n: Int) extends ShuffleOp
  case class CutNegative(n: Int) extends ShuffleOp
  case object DealIntoStack extends ShuffleOp
  case class DealWithIncrement(n: Int) extends ShuffleOp



  object ShuffleOp {
    def apply(op: String): ShuffleOp =  op match {
      case incrementR(i) => DealWithIncrement(i.toInt)
      case "deal into new stack" => DealIntoStack
      case cut(i) => Cut(i.toInt)
      case cutNegative(i) => CutNegative(i.toInt)
      case _ => ???
    }
  }
}

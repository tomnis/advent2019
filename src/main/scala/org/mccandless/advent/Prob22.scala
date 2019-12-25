package org.mccandless.advent

import java.math.BigInteger

import org.mccandless.advent.Prob22Types.{Card, Cut, CutNegative, DealIntoStack, DealWithIncrement, Deck, LinComb, ShuffleOp}
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
  def combineRev(ops: Seq[ShuffleOp], deckSize: BigInt): LinComb = {

    ops.foldRight(LinComb.identity) {
      case (DealIntoStack, acc) => acc.copy(a = acc.a * -1, b = deckSize - acc.b - 1 )
      case (Cut(n), acc) => acc.copy(b = (acc.b + n).mod(deckSize))
      case (CutNegative(n), acc) => acc.copy(b = (acc.b - n).mod(deckSize))
      case (DealWithIncrement(inc), acc) => {
        /**
         * z = pow(n,L-2,L) # == modinv(n,L)
         * a = a*z % L
         * b = b*z % L
         */
        // n ^ (L - 2) % L
        val z: BigInt = pow(inc, deckSize - 2, deckSize)
        acc.copy(
          a = (acc.a * z).mod(deckSize),
          b = (acc.b * z).mod(deckSize)
        )
      }
    }
  }

  def pow(base: BigInt, exp: BigInt, mod: BigInt): BigInt = {
//    var t: Long = 1L
//    var curBase: Long = base
//    var curExp: Long = exp
//
//    while (curExp > 0) {
//
//      if (curExp % 2 != 0) {
//        t = (t * curBase) % mod
//      }
//
//      curBase = (curBase * curBase) % mod;
//      curExp /= 2;
//    }
//
//    t % mod
    base.modPow(exp, mod)
  }

  println(pow(36, 119315717514045L,119315717514047L))
  require(pow(36, 119315717514045L,119315717514047L) == 43086231324517L)


  // (ax+b)^m % n
  def polyPow(c: LinComb, m: BigInt, n: BigInt): LinComb = {
//    if m==0:
//    return 1,0
//    if m%2==0:
//    return polypow(a*a%n, (a*b+b)%n, m//2, n)
//    else:
//    c,d = polypow(a,b,m-1,n)
//    return a*c%n, (a*d+b)%n
    if (m == BigInt(0)) {
      LinComb.identity
    }
    else if (m % 2 == 0L) {
      val newComb = c.copy(
        a = (c.a * c.a).mod(n),
        b = (c.a * c.b + c.b).mod(n)
      )
      polyPow(newComb, m / 2, n)
    }
    else {
      val newComb = polyPow(c, m - 1, n)
      newComb.copy(
        a = (c.a * newComb.a).mod(n),
        b = (c.a * newComb.b + c.b).mod(n)
      )
    }
  }




  def part2(ops: Seq[ShuffleOp]): BigInt = {
    val deckSize: BigInt = 119315717514047L
    val numShuffles: BigInt = 101741582076661L


    val l: LinComb = combineRev(ops, deckSize)
    val comb = polyPow(l, numShuffles, deckSize)

    (2020 * comb.a + comb.b) % deckSize
  }

  // 38728028849073 too low
  // 55722059438744 too low
  // 58059654365077 too low
  // 63686831161251 wrong
  println(part2(this.input().toSeq))
}


object Prob22Types {
  type Deck = Seq[Card]

  case class Card(number: Long)

  // ax + b
  case class LinComb(a: BigInt, b: BigInt)

  object LinComb {
    val identity: LinComb = LinComb(1, 0)
  }



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

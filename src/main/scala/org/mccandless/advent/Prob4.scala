package org.mccandless.advent

import scala.util.{Success, Try}

object Prob4 extends App {

  val min: Int = 265275
  val max: Int = 781584


  def meetsPasswordCriteria(password: Int): Boolean = {
    password.toString.length == 6 &&
      password >= min &&
      password <= max &&
      hasTwoAdjacentDigitsSame(password) &&
      digitsNeverDecrease(password)
  }
  require(meetsPasswordCriteria(266789))




  def hasTwoAdjacentDigitsSame(password: Int): Boolean = {
    val digits: Seq[Int] = password.toString.toCharArray.map(_.toInt).toSeq
    // filter the pairs of adjacent repeating digits
    digits.zipWithIndex.toList.map(LocatedDigit(_)).sliding(2).filter {
      case ld1 :: ld2 :: Nil => ld1.digit == ld2.digit
      // there should be at least one pair that is not part of a larger group
      // note however that a larger matching group may exist, eg 111122
    } exists {
      case ld1 :: ld2 :: Nil => matchingPairNotInLargerGroup(digits, ld1, ld2)
    }
  }
  require(hasTwoAdjacentDigitsSame(122345))
  require(!hasTwoAdjacentDigitsSame(123789))
  require(!hasTwoAdjacentDigitsSame(123444))
  require(!hasTwoAdjacentDigitsSame(124445))
  require(hasTwoAdjacentDigitsSame(111122))




  def matchingPairNotInLargerGroup(digits: Seq[Int], ld1: LocatedDigit, ld2: LocatedDigit): Boolean = {
    require(ld1.digit == ld1.digit)
    Try(digits(ld1.index - 1)) != Success(ld1.digit) && Try(digits(ld2.index + 1)) != Success(ld2.digit)
  }




  def digitsNeverDecrease(password: Int): Boolean = {
    password.toString.toCharArray.toList
      .map(_.toInt)
      .sliding(2)
      .forall(p => p(0) <= p(1))
  }
  require(digitsNeverDecrease(111123))
  require(digitsNeverDecrease(135679))




  val numPasswords: Int = (min to max).count(meetsPasswordCriteria)
  println(s" num passwords: $numPasswords")
//  require(numPasswords == 960)
}



// good idea to make a case class
case class LocatedDigit(digit: Int, index: Int)

object LocatedDigit {

  def apply(p: (Int, Int)): LocatedDigit = LocatedDigit(p._1, p._2)
}
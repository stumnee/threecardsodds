package threeCardOdds

/**
  * Strength
  *   - None
  *   - High Card
  *   - Pair
  *   - Flush
  *   - Straight
  *   - Trips
  *   - Straight Flush
  *
  * @param cards
  */

object Hand {
  val Suites = "cdhs"
  //                Twelve---+
  //                Eleven--+|
  //                   Ten-+||
  //             0123456789|||
  val Numbers = "23456789TJQKA"
  val Pair = 20000
  val Flush = 21000
  val Straight = 22000
  val Trips = 30000
  val StraightFlush = 40000
  val Royal = 50000

  def apply(cards: Array[Int]): Hand = {
    new Hand(cards)
  }

  def fromStr(s: String): Hand = {
    new Hand(s.split(" ").map(card => Numbers.indexOf(card.charAt(0)) + Suites.indexOf(card.charAt(1)) * 13).toArray )
  }
}
class Hand (cards: Array[Int]) {

  val flush = cards.map(_ / 13).distinct.length == 1
  val sorted = cards.map(_ % 13).sortWith((a, b) => a < b)

  val trips = sorted.distinct.length == 1
  val pair = sorted.distinct.length == 2

  val wheel = (sorted(0) + 1 == sorted(1) && sorted(0) + 12 == sorted(2))
  val straight = (sorted(0) + 1 == sorted(1) && sorted(1) + 1 == sorted(2)) || wheel

  var strength = if (wheel) {
    13 * sorted(0) + 169 * sorted(1)
  } else if (pair && sorted(0) == sorted(1)) { // example 22A
    sorted(2) + sorted(0) * 13 + sorted(1) * 169
  } else {
    sorted(0) + sorted(1) * 13 + sorted(2) * 169
  }

  if (isMiniRoyal()) {
    strength += Hand.Royal
  } else if (isStraightFlush()) {
    strength += Hand.StraightFlush
  } else if (isTrips()) {
    strength += Hand.Trips
  } else if (isStraight()) {
    strength += Hand.Straight
  } else if (isFlush()) {
    strength += Hand.Flush
  } else if (isPair()) {
    strength += Hand.Pair
  }

  def handStrength(): Int = {
    strength
  }

  def isQualified(): Boolean = {
    strength > 169 * Hand.Numbers.indexOf("Q") // at least a Queen
  }

  def isPair(): Boolean = {
    pair
  }

  def isTrips(): Boolean = {
    trips
  }

  def isFlush(): Boolean = {
    flush
  }

  def isStraight(): Boolean = {
    straight
  }

  def isStraightFlush(): Boolean = {
    straight && flush
  }

  def isMiniRoyal(): Boolean = {
    straight && flush && sorted(2) == 12 //Ace
  }

  override def toString: String = {
    cards.map(c => Hand.Numbers.charAt(c % 13).toString +  Hand.Suites.charAt(c / 13)).mkString(" ") + " --  " + strength
  }

}
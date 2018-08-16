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
  val Symbols = "23456789TJQKA"
  val Pair = 20000
  val Flush = 21000
  val Straight = 22000
  val Trips = 30000
  val StraightFlush = 40000
  val Royal = 50000

  def apply(cards: Array[Int]): Hand = {
    new Hand(cards)
  }

  def symbolValue(ch: Char): Int = {
    Symbols.indexOf(ch)
  }
  def symbolValue(s: String): Int = {
    Symbols.indexOf(s)
  }
  def fromStr(s: String): Hand = {
    new Hand(s.split(" ").map(card => symbolValue(card.charAt(0)) + Suites.indexOf(card.charAt(1)) * 13).toArray )
  }
}

class Hand (cards: Array[Int]) {

  val flush = cards.map(_ / 13).distinct.length == 1
  val sorted = cards.map(_ % 13).sortWith((a, b) => a < b)

  val trips = sorted.distinct.length == 1
  val pair = sorted.distinct.length == 2

  val wheel = sorted(0) == 0 && sorted(1) == 1 && sorted(2) == 12
  val straight = (sorted(0) + 1 == sorted(1) && sorted(1) + 1 == sorted(2)) || wheel

  var score = if (wheel) {
    13 * sorted(0) + 169 * sorted(1)
  } else if (pair && sorted(0) == sorted(1)) { // example 22A
    sorted(2) + sorted(0) * 13 + sorted(1) * 169
  } else {
    sorted(0) + sorted(1) * 13 + sorted(2) * 169
  }

  if (isMiniRoyal()) {
    score += Hand.Royal
  } else if (isStraightFlush()) {
    score += Hand.StraightFlush
  } else if (isTrips()) {
    score += Hand.Trips
  } else if (isStraight()) {
    score += Hand.Straight
  } else if (isFlush()) {
    score += Hand.Flush
  } else if (isPair()) {
    score += Hand.Pair
  }

  def strength(): Int = {
    score
  }

  def upCardValue(): Int = {
    cards(0) % 13
  }

  def isUpCardQueenOrBetter(): Boolean = {
    upCardValue() >= 10
  }

  def isQualified(): Boolean = {
    score > 169 * Hand.symbolValue("Q") // at least a Queen
  }

  def cardValues(): Array[Int] = {
    sorted
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

  def isWheel(): Boolean = {
    wheel
  }

  def isStraightFlush(): Boolean = {
    straight && flush
  }

  def isMiniRoyal(): Boolean = {
    straight && flush && sorted(0) == Hand.symbolValue("Q") //QKA
  }

  def isRaisable(upCard: Int): Boolean = {
    if (upCard < Hand.symbolValue("Q") || score > Hand.Pair) {
      return true
    }

    if (upCard < sorted(2)) {
      return true
    }

    return upCard == sorted(2) && sorted(1) > Hand.symbolValue("9") // second best card 9 or better
  }

  override def toString: String = {
    cards.map(c => Hand.Symbols.charAt(c % 13).toString +  Hand.Suites.charAt(c / 13)).mkString(" ")
  }

}
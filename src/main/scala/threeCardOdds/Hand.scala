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

  def apply(cards: Array[Int]): Hand = {
    new Hand(cards)
  }

  def apply(s: String): Hand = {
    new Hand(s)
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

  // auxiliary constructor
  def this(s: String) = this(s.split(" ").map(card => Hand.symbolValue(card.charAt(0)) + Hand.Suites.indexOf(card.charAt(1)) * 13).toArray )

  val sorted = cards.map(_ % 13).sortWith((a, b) => a < b)

  var power = PairPlus.None

  if (sorted.distinct.length == 2) {
    power = PairPlus.Pair
  }
  if (cards.map(_ / 13).distinct.length == 1) {
    power = PairPlus.Flush
  }

  val wheel = sorted(0) == 0 && sorted(1) == 1 && sorted(2) == 12
  if ((sorted(0) + 1 == sorted(1) && sorted(1) + 1 == sorted(2)) || wheel) {
    power = if (power == PairPlus.Flush) PairPlus.StraightFlush else PairPlus.Straight
  }

  if (power == PairPlus.StraightFlush && sorted(0) == Hand.symbolValue("Q")) { //QKA
    power = PairPlus.Royal
  }

  if (sorted.distinct.length == 1) {
    power = PairPlus.Trips
  }

  var score = if (wheel) {
    13 * sorted(0) + 169 * sorted(1)
  } else if (power == PairPlus.Pair && sorted(0) == sorted(1)) { // example 22A
    sorted(2) + sorted(0) * 13 + sorted(1) * 169
  } else {
    sorted(0) + sorted(1) * 13 + sorted(2) * 169
  }

  score += power.getScore()

  def strength(): Int = {
    score
  }

  def getPower(): PairPlusType = {
    power
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

  def isRaisable(upCard: Int): Boolean = {
    if (upCard < Hand.symbolValue("Q") || score > PairPlus.Pair.getScore) {
      return true
    }

    if (upCard < sorted(2)) {
      return true
    }

    return upCard == sorted(2) && sorted(1) > Hand.symbolValue("9") // second best card 9 or better
  }

  def getCards(): Array[Int] = {
    cards
  }

  override def toString: String = {
    cards.map(c => Hand.Symbols.charAt(c % 13).toString +  Hand.Suites.charAt(c / 13)).mkString(" ")
  }

}
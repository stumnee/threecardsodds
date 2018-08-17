package threeCardOdds


class SixCardsType (kind: String, value: Int) {
  override def toString: String = kind

  def getValue(): Int = {
    value
  }
}

object SixCardsTypes extends Enumeration {
  val None = new SixCardsType("none", 0)
  val Trips = new SixCardsType("trips", 8)
  val Straight = new SixCardsType("straight", 9)
  val Flush = new SixCardsType("flush", 15)
  val FullHouse = new SixCardsType("full house", 20)
  val Quads = new SixCardsType("quad", 100)
  val StraightFlush = new SixCardsType("straight flush", 200)
  val RoyalFlush = new SixCardsType("royal flush", 1000)
}

object SixCards {
  def apply(dealer: Hand, player: Hand): SixCards = new SixCards(dealer, player)
}

class SixCards (dealer: Hand, player: Hand) {
  var power = SixCardsTypes.None

  val cards = dealer.getCards() ++ player.getCards()

  val sorted = cards.map(_ % 13).sortWith((a, b) => a < b)


  if (cards.map(_ / 13).groupBy(it=>it).mapValues(_.size).filter(_._2 > 4).size > 0) {
    power = SixCardsTypes.Flush
  }

  val straight = getStraight(sorted.distinct)

  if (straight.length == 5) {
    if (power == SixCardsTypes.Flush) {
      //check for straight flush
      val sf = cards.filter(it=>straight.indexOf(it % 13) > -1).map(_ / 13).groupBy(it=>it).mapValues(_.size)
      if (sf.filter(_._2 > 4).size > 0) {
        power = SixCardsTypes.StraightFlush
        if (straight(3) == Hand.symbolValue("K")) { //KA
          power = SixCardsTypes.RoyalFlush
        }
      }
    } else {
      power = SixCardsTypes.Straight
    }
  }

  val grouped = sorted.groupBy(it=>it).mapValues(_.size)
  if (grouped.filter(_._2  == 3).size == 1) {
    if (grouped.filter(_._2  == 2).size == 1) {
      power = SixCardsTypes.FullHouse
    } else {
      power = SixCardsTypes.Trips
    }
  }

  if (grouped.filter(_._2  == 3).size == 2) {
    power = SixCardsTypes.FullHouse
  }

  if (grouped.filter(_._2  == 4).size > 0) {
    power = SixCardsTypes.Quads
  }

  def getPower(): SixCardsType = {
    power
  }

  def getPowerValue(): Int = {
    power.getValue()
  }

  def isStraight(cards: Array[Int]): Boolean = {
    cards(0) + 4 == cards(4) || isWheel(cards)
  }
  def isWheel(cards: Array[Int]): Boolean = {
    cards(0) == 0 && cards(4) == 12 && (cards(3) - cards(0) == 3)
  }

  def getStraight(cards: Array[Int]): Array[Int] = {
    if (cards.length == 5 && isStraight(cards)) {
      return cards
    }

    if (cards.length > 5) {

      if (isStraight(cards.slice(1, 6)))
        return cards.slice(1, 6)

      if (isStraight(cards.slice(0, 5)))
        return cards.slice(0, 5)

      if (isWheel(cards.slice(0, 4) ++ Array(cards(5))))
        return cards.slice(0, 4) ++ Array(cards(5))
    }
    return Array[Int]()
  }

  override def toString: String = {
    cards.sortWith((a, b) => a % 13 < b % 13).map(c => Hand.Symbols.charAt(c % 13).toString +  Hand.Suites.charAt(c / 13)).mkString(" ")
  }
}
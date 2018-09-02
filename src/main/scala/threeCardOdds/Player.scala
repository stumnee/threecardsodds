package threeCardOdds

import scala.collection.mutable.ListBuffer

object Player {
  var dealer = new Dealer()

  var pool = List[Player]()

  def apply(): Player = new Player()

  def spawn(num: Int): Unit = {
    pool = dealer +: (1 to num).map{idx=>new Player(idx)}.toList
  }

  def onlyPlayers(): List[Player] = {
    pool.filter(_.idx > 0)
  }

}

class Dealer() extends Player {
  this.idx = 0

  var qualifiedCount = 0
  var upcardQueenOrBetterCount = 0
  var upcardLessThanQueenQualifiedCount = 0


  override def calc() = {
    qualifiedCount += (if (this.hand.isQualified()) 1 else 0)
    if (this.hand.upCardValue() >= Hand.symbolValue("Q")) {
      upcardQueenOrBetterCount += 1
    } else {
      upcardLessThanQueenQualifiedCount += (if (this.hand.isQualified()) 1 else 0)
    }
  }
}
class Player(var idx: Int = -1) {

  var pairPlus = ListBuffer[BonusType]()
  var sixCards = ListBuffer[BonusType]()

  var betAnte = 0
  var betRaise = 0

  var hand: Hand = null

  def calc(): Unit = {
    if (Player.dealer.hand.isQualified()) {
      if (this.hand.isRaisable(Player.dealer.hand.upCardValue())) {
        betRaise += (if (Player.dealer.hand.strength > this.hand.strength) -1 else 1)
      }

      betAnte += (if (Player.dealer.hand.strength > this.hand.strength) -1 else 1)
    } else {
      betAnte += 1
    }

    pairPlus += hand.getPower()
    sixCards += new SixCards(Player.dealer.hand, this.hand).getPower
  }

  override def toString: String = {
    if (this.idx == 0)
      "Dealer"
    else
      "Player " + this.idx
  }
}
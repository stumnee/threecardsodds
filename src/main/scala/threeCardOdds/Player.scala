package threeCardOdds

import scala.collection.mutable.ListBuffer

object Player {
  var pool = List[Player]()
  def apply(): Player = new Player()

  def spawn(num: Int): Unit = {
    pool = (0 to num).map{idx=>new Player(idx, idx == 0)}.toList

    Player.dealer = pool.filter(_.isDealer).head
  }

  def onlyPlayers(): List[Player] = {
    pool.filter(!_.isDealer)
  }

  var dealer: Player = null
}

class Player(var idx: Int = -1, var isDealer: Boolean = false) {

  var pairPlus = ListBuffer[PairPlusType]()
  var sixCards = ListBuffer[SixCardsType]()

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
    if (this.isDealer)
      "Dealer"
    else
      "Player " + this.idx
  }

  def printWinLoss(): Unit = {

    println(this)

    val max = pairPlus.size

    println(s"Ante = $betAnte; Raise=$betRaise; Net = ${betAnte + betRaise}")



    val pp = pairPlus.groupBy(it=>it).mapValues(_.size)

    println("Pair Plus: " + pp)

//    val pairPlusValues = (pp.groupBy(it=>it).map{case(k,v)=> (k,v.size * (k.getValue() + 1))})
//
//    println(pairPlusValues)
//    println((pairPlusValues.map(_._2).sum - max)*100.0/max + "%")
////
//
//
//
    val sc = sixCards.groupBy(it=>it).mapValues(_.size)

    println("\nSix Cards: " + sc)
//
//    println(sixCardsOdds.map{case(k,v)=>(k,v * 100.0/max)})
//
//
//    val scValues = (sc.groupBy(it=>it).map{case(k,v)=> (k,v.size * (k.getValue() + 1))})
//
//    println(scValues)
//
//
//    println((scValues.map(_._2).sum - max)*100.0/max + "%")

    println("\n\n")
  }
}
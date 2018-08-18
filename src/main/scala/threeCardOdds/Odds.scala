package threeCardOdds

import scala.collection.mutable.ListBuffer

object Main extends App {
  var betAnte = 0
  var betRaise = 0

  var pp = ListBuffer[PairPlusType]()
  var sc = ListBuffer[SixCardsType]()

  val max = 100000

  for (i<-0 to max) {
    val deck = Deck()

    val dealer = new Hand(deck.nextThree())
    val player = new Hand(deck.nextThree())

    if (dealer.isQualified()) {
      if (player.isRaisable(dealer.upCardValue())) {
        betRaise += (if (dealer.strength > player.strength) -1 else 1)
      }

      betAnte += (if (dealer.strength > player.strength) -1 else 1)
    } else {
      betAnte += 1
    }

    if (player.getPower() != PairPlus.None) {
      pp += player.getPower()
    }

    val sixCards = new SixCards(dealer, player)

    if (sixCards.getPower() != SixCardsTypes.None) {
      sc += sixCards.getPower
    }

  }

  println(s"Ante = $betAnte; Raise=$betRaise; Net = ${betAnte + betRaise}")




  val pairPlus = pp.groupBy(it=>it).mapValues(_.size)

  println(pairPlus, pairPlus.map(_._2).sum)


  val pairPlusValues = (sc.groupBy(it=>it).map{case(k,v)=> (k,v.size * (k.getValue() + 1))})
  println(pairPlusValues)
  println((pairPlusValues.map(_._2).sum - max)*100.0/max + "%")




  val sixCardsOdds = sc.groupBy(it=>it).mapValues(_.size)

  println(sixCardsOdds)

  println(sixCardsOdds.map{case(k,v)=>(k,v * 100.0/max)})


  val scValues = (sc.groupBy(it=>it).map{case(k,v)=> (k,v.size * (k.getValue() + 1))})

  println(scValues)


  println((scValues.map(_._2).sum - max)*100.0/max + "%")

}

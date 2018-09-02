package threeCardOdds

import scala.collection.mutable.ListBuffer

object Main extends App {
  val Max = 100000
  val MaxPlayers = 6

  Player.spawn(MaxPlayers)

  for (i<-1 to Max) {
    val deck = Deck()

    for (player <- Player.pool) {
      player.hand = new Hand(deck.nextThree())
    }


    for (player <- Player.onlyPlayers) {
      player.calc()
    }

    Player.dealer.calc()
  }

  var (totalAnte, totalRaise, total) = (0, 0, 0)

  // print each stats
  for (player <- Player.onlyPlayers) {
    println(player)

    val net = player.betAnte + player.betRaise

    totalAnte += player.betAnte
    totalRaise += player.betRaise
    total += net

    println(s"Ante = $player.betAnte; Raise=$player.betRaise; Net = $net/${Bonus.percent(net, Max)} ")

    println("Pair Plus: " + Bonus.bonus(player.pairPlus.toList))

    println("Six Cards: " + Bonus.bonus(player.sixCards.toList))

  }


  println("\n\n*** All combined")
  println(s"Ante = $totalAnte; Raise=$totalRaise; Total = $total/${Bonus.percent(total, Max * MaxPlayers)} ")

  println("Pair Plus: " + Bonus.bonus(Player.onlyPlayers.map(it=>it.pairPlus).flatMap(it=>it)))
  println("Six Cards: " + Bonus.bonus(Player.onlyPlayers.map(it=>it.sixCards).flatMap(it=>it)))


  println("\n\n*** Dealer")
  println("Qualified=" + Player.dealer.qualifiedCount + "/" + Bonus.percent(Player.dealer.qualifiedCount, Max))
  println("Upcard Q or Better=" + Player.dealer.upcardQueenOrBetterCount)
  println("Qualified with upcard<Q=" + Player.dealer.upcardLessThanQueenQualifiedCount + "/" + Bonus.percent(Player.dealer.upcardLessThanQueenQualifiedCount, Max-Player.dealer.upcardQueenOrBetterCount))

}


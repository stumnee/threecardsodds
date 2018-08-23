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
  }

  var (totalAnte, totalRaise, total) = (0, 0, 0)

  // print each stats
  for (player <- Player.onlyPlayers) {
    println(player)

    val net = player.betAnte + player.betRaise

    totalAnte += player.betAnte
    totalRaise += player.betRaise
    total += net

    println(s"Ante = $player.betAnte; Raise=$player.betRaise; Net = $net/${net * 100.0 / Max}%; ")

    println("Pair Plus: " + Bonus.bonus(player.pairPlus.toList))

    println("Six Cards: " + Bonus.bonus(player.sixCards.toList))

    println("\n\n")
  }


  println("*** All combined")
  println(s"Ante = $totalAnte; Raise=$totalRaise; Total = $total/${total * 100.0 / Max}%; ")

  println("Pair Plus: " + Bonus.bonus(Player.onlyPlayers.map(it=>it.pairPlus).flatMap(it=>it)))
  println("Six Cards: " + Bonus.bonus(Player.onlyPlayers.map(it=>it.sixCards).flatMap(it=>it)))
}


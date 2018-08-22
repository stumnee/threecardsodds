package threeCardOdds

import scala.collection.mutable.ListBuffer

object Main extends App {
  val Max = 1000
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

  for (player <- Player.onlyPlayers) {
    player.printWinLoss()
  }

}


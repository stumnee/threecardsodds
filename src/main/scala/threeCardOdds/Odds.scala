package threeCardOdds

object Main extends App {
  var betAnte = 0
  var betRaise = 0

  for (i<-0 to 10) {
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

    println(s"$dealer ~~ $player  -- ${dealer.strength} ?? ${player.strength} ${dealer.isQualified()}  Ante = $betAnte; Raise=$betRaise; Net = ${betAnte + betRaise}")
  }
}

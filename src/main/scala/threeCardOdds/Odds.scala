package threeCardOdds

import scala.collection.mutable.ListBuffer

object Main extends App {
  var betAnte = 0
  var betRaise = 0
  var pair = 0
  var flush = 0
  var straight = 0
  var trips = 0
  var sflush = 0
  var royal = 0

  var pp = ListBuffer[String]()

  val max = 1000000

  for (i<-0 to max) {
    val deck = Deck()

    val dealer = new Hand(deck.nextThree())
    val player = new Hand(deck.nextThree())

    var power = ""
    if (player.isPair) {power = "pair"}
    if (player.isFlush) {power = "flush"}
    if (player.isStraight) {power = "straight"}
//    if (player.isWheel) {power = "wheel"}
    if (player.isTrips) {power = "trips"}
    if (player.isStraightFlush) {power = "sflush"}
    if (player.isMiniRoyal) {power = "royal"}

    if (dealer.isQualified()) {
      if (player.isRaisable(dealer.upCardValue())) {
        betRaise += (if (dealer.strength > player.strength) -1 else 1)
      }

      betAnte += (if (dealer.strength > player.strength) -1 else 1)
    } else {
      betAnte += 1
    }

//    if (power == "royal")
//    println(s"$dealer ~~ $player  -- ${dealer.strength} ?? ${player.strength} ${dealer.isQualified()} $power")

    if (!power.isEmpty)
      pp += power



  }

  println(s"Ante = $betAnte; Raise=$betRaise; Net = ${betAnte + betRaise}")

  val pairPlus = pp.groupBy(it=>it).mapValues(_.size)

  println(pairPlus, pairPlus.map(_._2).sum)

  val bonus = pairPlus.map { case (b, c) =>
    b match {
      case "pair" => c * 1 + c
      case "flush" => c * 3 + c
      case "straight" => c * 6 + c
      case "wheel" => c * 6 + c
      case "trips" => c * 30 + c
      case "sflush" => c * 40 + c
      case "royal" => c * 200 + c
    }
  }.sum
  println(s"bonus=$bonus net=${bonus - max} ${(bonus - max)* 100.0/max}%")


}

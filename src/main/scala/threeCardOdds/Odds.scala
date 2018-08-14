package threeCardOdds

import scala.collection.mutable.ListBuffer



class Deck() {
  var choices = ListBuffer.tabulate(52)(_ + 0)
  val r = scala.util.Random
  var cards = ListBuffer[Int]()

  while (choices.size > 0) {
    cards.append(choices.remove(r.nextInt(choices.size)))
  }

  def nextThree(): Array[Int] = {
    return Array[Int](cards.remove(0), cards.remove(0), cards.remove(0))
  }
//  println(cards)
}

object Main extends App {
  val deck = new Deck()

  val sixCards = new SixCards()

  val dealer = new Hand(deck.nextThree())
  val player = new Hand(deck.nextThree())

}

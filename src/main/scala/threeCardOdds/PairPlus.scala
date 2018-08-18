package threeCardOdds

class PairPlusType(name: String, score: Int, payout: Int) {
  def getScore(): Int = {
    score
  }

  override def toString: String = name
}

object PairPlus extends Enumeration {
  val None = new PairPlusType("none", 0, 0)
  val Pair = new PairPlusType("pair", 20000, 1)
  val Flush = new PairPlusType("flush", 21000, 3)
  val Straight = new PairPlusType("straight", 22000, 6)
  val Trips = new PairPlusType("three of a kind", 30000, 30)
  val StraightFlush = new PairPlusType("straight flush", 40000, 40)
  val Royal = new PairPlusType("mini royal flush", 50000, 200)

}
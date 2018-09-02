package threeCardOdds

import scala.collection.mutable.ListBuffer


object Bonus {
  def percent(v: Int, max: Int) : String = {
    (math.round(v*10000.0/max) / 100.0) + "%"
  }

  def bonus (list: List[BonusType]): String = {
    val max = list.size
    val bonusGroupped = list.groupBy(it=>it).mapValues(_.size) //Map[BonusType, Int]

    val bonusVal = bonusGroupped.map{case(k,v)=> v * (k.getValue() + (if (k.getValue() == 0) -1 else 0))}.sum
    bonusGroupped.mapValues(v=>s"$v/${percent(v, max)}%") + s" total=$bonusVal/${percent(bonusVal, max)}%"
  }
}
abstract class Bonus {

}

trait BonusType {
  def getValue(): Int
}
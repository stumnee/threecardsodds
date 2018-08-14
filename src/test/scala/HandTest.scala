package threeCardOdds


import org.scalatest.FunSuite

class HandTest extends FunSuite {
  test("Flush Test") {
    //3C 4C 5C
    assert(new Hand(Array(1,2,3)).isFlush() == true)

    assert(new Hand(Array(1,2,13)).isFlush() == false)

    assert(Hand(Array(1,2,12)).isFlush() == true)
  }

  test("Straight Test") {
    //3C 4C 5C
    assert(new Hand(Array(1,2,3)).isStraight() == true)

    // 2D 3C 4C
    assert(new Hand(Array(1,2,13)).isStraight() == true)

    // 3C 4C 7C
    assert(new Hand(Array(1,2,5)).isStraight() == false)
  }

  test("Wheel Test") {
    // AD 2C 3C
    assert(new Hand(Array(0,1,12)).isStraight() == true)
  }

  test("Straight Flush Test") {
    //3C 4C 5C
    assert(new Hand(Array(1,2,3)).isStraightFlush() == true)

    // 2D 3C 4C
    assert(new Hand(Array(1,2,13)).isStraightFlush() == false)

    // 2C 3C 4C
    assert(new Hand(Array(0,1,2)).isStraightFlush() == true)

    // AC 2C 3C
    assert(new Hand(Array(12,1,2)).isStraightFlush() == false)
  }

  test("Mini Royal Flush Test") {
    //3C 4C 5C
    assert(new Hand(Array(1,2,3)).isMiniRoyal() == false)

    // 2D 3C 4C
    assert(new Hand(Array(1,2,13)).isMiniRoyal() == false)

    // 2C 3C 4C
    assert(new Hand(Array(0,1,2)).isMiniRoyal() == false)

    // AC 2C 3C
    assert(new Hand(Array(12,1,2)).isMiniRoyal() == false)

    // AC QC KC
    assert(new Hand(Array(12,10,11)).isMiniRoyal() == true)

    // QC KC AC
    assert(new Hand(Array(10,11,12)).isMiniRoyal() == true)

    // QD KD AD
    assert(new Hand(Array(23,24,25)).isMiniRoyal() == true)

    // QH KH AH
    assert(new Hand(Array(36,37,38)).isMiniRoyal() == true)

    // QS KS AS
    assert(new Hand(Array(49,50,51)).isMiniRoyal() == true)

    // QH KS AS
    assert(new Hand(Array(36,50,51)).isMiniRoyal() == false)
  }

  test("Pair Test") {
    //3C 4C 5C
    assert(new Hand(Array(1,2,3)).isPair() == false)

    // AC 2C 3C
    assert(new Hand(Array(12,1,2)).isPair() == false)

    // 2D 3C 4C
    assert(new Hand(Array(1,2,13)).isPair() == false)

    // 2C 3C 4C
    assert(new Hand(Array(0,1,2)).isPair() == false)

    // 2C 3C 2D
    assert(new Hand(Array(0,1,13)).isPair() == true)

    // 2C 4C 4D
    assert(new Hand(Array(0,2,15)).isPair() == true)
  }

  test("Trips Test") {
    //3C 4C 5C
    assert(new Hand(Array(1,2,3)).isTrips() == false)

    // AC 2C 3C
    assert(new Hand(Array(12,1,2)).isTrips() == false)

    // 2D 3C 4C
    assert(new Hand(Array(1,2,13)).isTrips() == false)

    // 2C 3C 4C
    assert(new Hand(Array(0,1,2)).isTrips() == false)

    // 2C 2H 2D
    assert(new Hand(Array(0,26,13)).isTrips() == true)

    // 4S 4C 4D
    assert(new Hand(Array(41,2,15)).isTrips() == true)
  }

  test("Strength Test") {
    assert(Hand.fromStr("3c 8d 7s").handStrength == 6 * 169 + 5 * 13 + 1)
    assert(Hand.fromStr("3c 8d 2s").handStrength == 6 * 169 + 1 * 13 + 0)
    assert(Hand.fromStr("3c 8d Qd").handStrength == 10 * 169 + 6 * 13 + 1)
    assert(Hand.fromStr("3c 4s 5s").handStrength == 3 * 169 + 2 * 13 + 1 + Hand.Straight)
    assert(Hand.fromStr("3c 8d As").handStrength == 12 * 169 + 6 * 13 + 1)
    assert(Hand.fromStr("3s 8s As").handStrength == 12 * 169 + 6 * 13 + 1 + Hand.Flush)
    assert(Hand.fromStr("3s 3d 3h").handStrength == 1 * 169 + 1 * 13 + 1 + Hand.Trips)
    assert(Hand.fromStr("3s 3d Ad").handStrength == 1 * 169 + 1 * 13 + 12 + Hand.Pair)
    assert(Hand.fromStr("4s 4d Kd").handStrength == 2 * 169 + 2 * 13 + 11 + Hand.Pair)
    assert(Hand.fromStr("4s 4d Kd").handStrength > Hand.fromStr("3s 3d Ad").handStrength)
    assert(Hand.fromStr("4s 4d Kd").handStrength > Hand.fromStr("4h 4s Qd").handStrength)
    assert(Hand.fromStr("4s 4d Qh").handStrength == Hand.fromStr("4h 4s Qd").handStrength)
    assert(Hand.fromStr("4s 4d Qh").handStrength == Hand.fromStr("Qs 4h 4s").handStrength)
  }

  test("Qualify Test") {
    assert(Hand.fromStr("3c 8d 7s").isQualified == false)
    assert(Hand.fromStr("3c 8d 2s").isQualified == false)
    assert(Hand.fromStr("3c 8d Qd").isQualified == true)
    assert(Hand.fromStr("3c 4s 5s").isQualified == true)
    assert(Hand.fromStr("3c 8d As").isQualified == true)
    assert(Hand.fromStr("3s 8s As").isQualified == true)
    assert(Hand.fromStr("3s 3d 3h").isQualified == true)
    assert(Hand.fromStr("3s 3d Ad").isQualified == true)
    assert(Hand.fromStr("4s 4d Kd").isQualified == true)
    assert(Hand.fromStr("4s 4d Kd").isQualified == true)
    assert(Hand.fromStr("4s 4d Kd").isQualified == true)
    assert(Hand.fromStr("4s 4d Qh").isQualified == true)
    assert(Hand.fromStr("4s 4d Qh").isQualified == true)
  }
}
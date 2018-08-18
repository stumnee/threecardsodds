package threeCardOdds


import org.scalatest.FunSuite

class HandTest extends FunSuite {
  test("Flush Test") {
    //3C 4C 5C
    assert(new Hand(Array(1,2,7)).getPower() == PairPlus.Flush)

    assert(new Hand(Array(1,5,13)).getPower() == PairPlus.None)

    assert(Hand(Array(1,2,12)).getPower() == PairPlus.Flush)
  }

  test("Straight Test") {
    //3C 4C 5C
    assert(new Hand(Array(1,2,16)).getPower() == PairPlus.Straight)

    // 2D 3C 4C
    assert(new Hand(Array(1,2,13)).getPower() == PairPlus.Straight)

    // 3C 4C 7C
    assert(new Hand(Array(1,2,17)).getPower() == PairPlus.None)
  }

  test("Wheel Test") {
    // AD 2C 3C
    assert(new Hand(Array(0,1,25)).getPower() == PairPlus.Straight)
  }

  test("Straight Flush Test") {
    //3C 4C 5C
    assert(new Hand(Array(1,2,3)).getPower() == PairPlus.StraightFlush)

    // 2D 3C 4C
    assert(new Hand(Array(1,2,13)).getPower() == PairPlus.Straight)

    // 2C 3C 4C
    assert(new Hand(Array(0,1,2)).getPower() == PairPlus.StraightFlush)

    // AC 2C 3C
    assert(new Hand(Array(12,1,2)).getPower() == PairPlus.Flush)
  }

  test("Mini Royal Flush Test") {
    //3C 4C 5C
    assert(new Hand(Array(1,2,3)).getPower() == PairPlus.StraightFlush)

    // 2D 3C 4C
    assert(new Hand(Array(1,2,13)).getPower() == PairPlus.Straight)

    // 2C 3C 4C
    assert(new Hand(Array(0,1,2)).getPower() == PairPlus.StraightFlush)

    // AC 2C 3C
    assert(new Hand(Array(22,1,2)).getPower() == PairPlus.None)

    // AC QC KC
    assert(new Hand(Array(12,10,11)).getPower() == PairPlus.Royal)

    // QC KC AC
    assert(new Hand(Array(10,11,12)).getPower() == PairPlus.Royal)

    // QD KD AD
    assert(new Hand(Array(23,24,25)).getPower() == PairPlus.Royal)

    // QH KH AH
    assert(new Hand(Array(36,37,38)).getPower() == PairPlus.Royal)

    // QS KS AS
    assert(new Hand(Array(49,50,51)).getPower() == PairPlus.Royal)

    // QH KS AS
    assert(new Hand(Array(35,50,51)).getPower() == PairPlus.None)
  }

  test("Pair Test") {
    //3C 4C 5C
    assert(new Hand(Array(1,2,3)).getPower() == PairPlus.StraightFlush)

    // AC 2C 3C
    assert(new Hand(Array(19,1,2)).getPower() == PairPlus.None)

    // 2D 3C 4C
    assert(new Hand(Array(1,2,17)).getPower() == PairPlus.None)

    // 2C 3C 4C
    assert(new Hand(Array(0,1,2)).getPower() == PairPlus.StraightFlush)

    // 2C 3C 2D
    assert(new Hand(Array(0,1,13)).getPower() == PairPlus.Pair)

    // 2C 4C 4D
    assert(new Hand(Array(0,2,15)).getPower() == PairPlus.Pair)
  }

  test("Trips Test") {
    //
    assert(new Hand(Array(1,2,35)).getPower() == PairPlus.None)

    // AC 2C 3C
    assert(new Hand(Array(12,1,22)).getPower() == PairPlus.None)

    // 2D 3C 4C
    assert(new Hand(Array(1,2,33)).getPower() == PairPlus.None)

    // 2C 3C 4C
    assert(new Hand(Array(0,7,19)).getPower() == PairPlus.None)

    // 2C 2H 2D
    assert(new Hand(Array(0,26,13)).getPower() == PairPlus.Trips)

    // 4S 4C 4D
    assert(new Hand(Array(41,2,15)).getPower() == PairPlus.Trips)
  }

  test("Strength Test") {
    assert(Hand.fromStr("3c 8d 7s").strength == 6 * 169 + 5 * 13 + 1)
    assert(Hand.fromStr("3c 8d 2s").strength == 6 * 169 + 1 * 13 + 0)
    assert(Hand.fromStr("3c 8d Qd").strength == 10 * 169 + 6 * 13 + 1)
    assert(Hand.fromStr("3c 4s 5s").strength == 3 * 169 + 2 * 13 + 1 + PairPlus.Straight.getScore)
    assert(Hand.fromStr("3c 8d As").strength == 12 * 169 + 6 * 13 + 1)
    assert(Hand.fromStr("3s 8s As").strength == 12 * 169 + 6 * 13 + 1 + PairPlus.Flush.getScore)
    assert(Hand.fromStr("3s 3d 3h").strength == 1 * 169 + 1 * 13 + 1 + PairPlus.Trips.getScore)
    assert(Hand.fromStr("3s 3d Ad").strength == 1 * 169 + 1 * 13 + 12 + PairPlus.Pair.getScore)
    assert(Hand.fromStr("4s 4d Kd").strength == 2 * 169 + 2 * 13 + 11 + PairPlus.Pair.getScore)
    assert(Hand.fromStr("4s 4d Kd").strength > Hand.fromStr("3s 3d Ad").strength)
    assert(Hand.fromStr("4s 4d Kd").strength > Hand.fromStr("4h 4s Qd").strength)
    assert(Hand.fromStr("4s 4d Qh").strength == Hand.fromStr("4h 4s Qd").strength)
    assert(Hand.fromStr("4s 4d Qh").strength == Hand.fromStr("Qs 4h 4s").strength)
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

  test("Up card Queen or Better") {
    assert(Hand.fromStr("3c 8d 7s").isUpCardQueenOrBetter == false)
    assert(Hand.fromStr("3c 8d Qs").isUpCardQueenOrBetter == false)
    assert(Hand.fromStr("Js 8d Qs").isUpCardQueenOrBetter == false)
    assert(Hand.fromStr("4s 4d Kd").isUpCardQueenOrBetter == false)
    assert(Hand.fromStr("Qc 8d 7s").isUpCardQueenOrBetter == true)
    assert(Hand.fromStr("Kc 8d 7s").isUpCardQueenOrBetter == true)
    assert(Hand.fromStr("Ac 8d 7s").isUpCardQueenOrBetter == true)
  }

  test("Card values") {
    assert(Hand.fromStr("3c 8d 7s").cardValues()(2) == 6)
    assert(Hand.fromStr("Ac 8d 7s").cardValues().deep == Array(5, 6, 12).deep)
  }

  test("Is Raisable") {
    assert(Hand.fromStr("3c 8d 7s").isRaisable(Hand.symbolValue("A")) == false)
    assert(Hand.fromStr("3c 8d 7s").isRaisable(Hand.symbolValue("Q")) == false)
    assert(Hand.fromStr("3c 8d Ks").isRaisable(Hand.symbolValue("Q")) == true)
    assert(Hand.fromStr("3c 8d Ks").isRaisable(Hand.symbolValue("K")) == false)
    assert(Hand.fromStr("3c Td Ks").isRaisable(Hand.symbolValue("K")) == true)
    assert(Hand.fromStr("3c Td Qs").isRaisable(Hand.symbolValue("Q")) == true)
    assert(Hand.fromStr("3c 8d Qs").isRaisable(Hand.symbolValue("Q")) == false)

    assert(Hand.fromStr("3c 8d 7s").isRaisable(9) == true)

    assert(Hand.fromStr("3c 3d 7s").isRaisable(10) == true)
  }
}
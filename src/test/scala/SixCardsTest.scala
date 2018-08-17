package threeCardOdds


import org.scalatest.FunSuite

class SixCardsTest extends FunSuite {
  test("None Test") {
    assert(SixCards(new Hand("As Qd 2d"), Hand("3s 5h 3h")).getPower() == SixCardsTypes.None)

    assert(SixCards(Hand("2h 4h 5h"), Hand("6h 9s As")).getPower() == SixCardsTypes.None)
  }

  test("Trips Test") {
    assert(SixCards(Hand.fromStr("As Qd 3d"), Hand.fromStr("3s 5h 3h")).getPower() == SixCardsTypes.Trips)

    assert(SixCards(Hand.fromStr("As Qd 4d"), Hand.fromStr("3s 3h 3h")).getPower() == SixCardsTypes.Trips)

    assert(SixCards(Hand.fromStr("As Ad Ah"), Hand.fromStr("2s 5h 3h")).getPower() == SixCardsTypes.Trips)
  }

  test("Full House Test") {
    assert(SixCards(Hand("As Qd 3d"), Hand("3s Qh 3h")).getPower() == SixCardsTypes.FullHouse)

    assert(SixCards(Hand("As Ad Ac"), Hand.fromStr("3s 3h 3h")).getPower() == SixCardsTypes.FullHouse)

    assert(SixCards(Hand("As Ad Ah"), Hand.fromStr("3s 5h 3h")).getPower() == SixCardsTypes.FullHouse)
  }

  test("Quads Test") {
    assert(SixCards(Hand("As Qd 3d"), Hand("3s 3c 3h")).getPower() == SixCardsTypes.Quads)

    assert(SixCards(Hand("Qs Qd 7h"), Hand("7s Qc Qh")).getPower() == SixCardsTypes.Quads)
  }

  test("Striaght Test") {
    assert(SixCards(Hand("As 2d 3d"), Hand("4s 5c 3h")).getPower() == SixCardsTypes.Straight)

    assert(SixCards(Hand("As 9d 3d"), Hand("4s 5c 2h")).getPower() == SixCardsTypes.Straight)

    assert(SixCards(Hand("Qs Jd 9h"), Hand("Ts Kc Qh")).getPower() == SixCardsTypes.Straight)

    assert(SixCards(Hand("Qs Jd 9h"), Hand("Ts Kc Ah")).getPower() == SixCardsTypes.Straight)

  }

  test("Flush Test") {
    assert(SixCards(Hand("9d 2d 3d"), Hand("4d 5d 3h")).getPower() == SixCardsTypes.Flush)
    assert(SixCards(Hand("9d 2d 3d"), Hand("4d 5d Td")).getPower() == SixCardsTypes.Flush)

    //Flush over straight
    assert(SixCards(Hand("9d 8h 7d"), Hand("6d 5d 3d")).getPower() == SixCardsTypes.Flush)

    assert(SixCards(Hand("2h 4h 5h"), Hand("6h 9s Ah")).getPower() == SixCardsTypes.Flush)
  }

  test("Striaght Flush Test") {
    assert(SixCards(Hand("Ad 2d 3d"), Hand("4d 5d 3h")).getPower() == SixCardsTypes.StraightFlush)

    assert(SixCards(Hand("Ad 9d 3d"), Hand("4d 5d 2d")).getPower() == SixCardsTypes.StraightFlush)

    assert(SixCards(Hand("Qs Js 9s"), Hand("Ts Ks Qh")).getPower() == SixCardsTypes.StraightFlush)

  }

  test("Royal Flush Test") {
    assert(SixCards(Hand("Qh Jh 9h"), Hand("Th Kh Ah")).getPower() == SixCardsTypes.RoyalFlush)
    assert(SixCards(Hand("Qh Jh 8h"), Hand("Th Kh Ah")).getPower() == SixCardsTypes.RoyalFlush)
  }

}
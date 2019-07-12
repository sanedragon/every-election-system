package elections

class SchulzeElectionSpec extends BaseSpec {
  //TODO: handle ties
  "A RankedSchulzeElection" should "count correctly for a trivial case" in {
    val election = new RankedSchulzeElection(Set(alice, bob))

    val ballots = Seq(new RankedBallot(List(alice, bob)))

    val result = election.countBallots(ballots)

    result.rounds.size should be (1)

    val round = result.rounds.head

    round.schwartzSet should be (Set(alice))
  }

  it should "count correctly for a slightly less trivial case" in {
    val candidates = Set(alice, bob, carol, david)

    val election = new RankedSchulzeElection(candidates)

    val ballots =
      (1 to 4).map(_ => new RankedBallot(List(alice, bob, carol, david))) ++
        (1 to 2).map(_ => new RankedBallot(List(bob, carol, alice, david))) ++
        (1 to 3).map(_ => new RankedBallot(List(carol, alice, bob, david)))

    val result = election.countBallots(ballots)

    val firstRound = result.rounds.head

    firstRound.schwartzSet should be (Set(alice, bob, carol))
    firstRound.preferences.toList should be (List(
      Preference(carol, alice, 1),
      Preference(bob, carol, 3),
      Preference(alice, bob, 5)
    ))

    val secondRound = result.rounds(1)
    secondRound.schwartzSet should be (Set(alice, bob, carol))
    secondRound.preferences.toList should be (List(
      Preference(bob, carol, 3),
      Preference(alice, bob, 5)
    ))

    val thirdRound = result.rounds(2)
    thirdRound.schwartzSet should be (Set(alice, bob))
    thirdRound.preferences.toList should be (List(
      Preference(alice, bob, 5)
    ))

    val fourthRound = result.rounds(3)
    fourthRound.schwartzSet should be (Set(alice))
    fourthRound.preferences.toList should be (Nil)

    result.rounds.size should be (4)

  }

}

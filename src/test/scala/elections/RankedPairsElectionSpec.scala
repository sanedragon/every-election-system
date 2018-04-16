package elections

class RankedPairsElectionSpec extends BaseSpec {
  "A RankedBallotRankedPairsElection" should "count correctly for a trivial case" in {
    val candidates = Set(alice, bob)

    val election = new RankedBallotRankedPairsElection(candidates)

    val ballots = Set(new RankedBallot(election, List(alice, bob)))

    val result = election.countBallots(ballots)

    result.winner should be (alice)
    result.strongestPreferences.size should be (1)
    result.strongestPreferences.head should be (Preference(alice, bob, 1))
    result.lockedPreferences.edges.map(_.value) should be (Set(Preference(alice, bob, 1)))
    result.lockedPreferences.nodes.map(_.value) should be (candidates)
  }

  it should "count correctly for a slightly less trivial case" in {
    val candidates = Set(alice, bob, carol)

    val election = new RankedBallotRankedPairsElection(candidates)

    val ballots = (
        (1 to 4).map(_ => new RankedBallot(election, List(alice, bob, carol))) ++
        (1 to 2).map(_ => new RankedBallot(election, List(bob, carol, alice))) ++
        (1 to 3).map(_ => new RankedBallot(election, List(carol, alice, bob)))
      ).toSet

    val result = election.countBallots(ballots)

    result.strongestPreferences should be (List(
      Preference(alice, bob, 5),
      Preference(bob, carol, 3),
      Preference(carol, alice, 1)
    ))

    result.lockedPreferences.edges.map(_.value) should be (Set(
      Preference(alice, bob, 5),
      Preference(bob, carol, 3)
      // Carol over Alice would introduce a cycle
    ))
    result.lockedPreferences.nodes.map(_.value) should be (candidates)

    result.winner should be (alice)
  }

  "A ScoreBallotRankedPairsElection" should "count correctly for a trivial case" in {
    val candidates = Set(alice, bob)

    val election = new ScoreBallotRankedPairsElection(candidates)

    val ballots = Set(new ScoreBallot(election, Map(alice -> 1, bob -> 0)))

    val result = election.countBallots(ballots)

    result.winner should be (alice)
    result.strongestPreferences.size should be (1)
    result.strongestPreferences.head should be (Preference(alice, bob, 1))
    result.lockedPreferences.edges.map(_.value) should be (Set(Preference(alice, bob, 1)))
    result.lockedPreferences.nodes.map(_.value) should be (candidates)
  }

  it should "count correctly for a slightly less trivial case" in {
    val candidates = Set(alice, bob, carol)

    val election = new ScoreBallotRankedPairsElection(candidates)

    val ballots = Set(
        new ScoreBallot(election, Map(alice -> 10, bob -> 0)),
        new ScoreBallot(election, Map(alice -> 6, bob -> 10, carol -> 0)),
        new ScoreBallot(election, Map(alice -> 0, carol -> 10))
      )

    val result = election.countBallots(ballots)

    result.strongestPreferences.size should be (3)

    result.strongestPreferences(0) should be (Preference(bob, carol, 1.0))
    result.strongestPreferences(1) should be (Preference(alice, bob, 0.6))
    result.strongestPreferences(2) should be (Preference(carol, alice, 0.4))

    result.lockedPreferences.edges.map(_.value) should be (Set(
      Preference(alice, bob, 1.5),
      Preference(bob, carol, 1.0)
      // Carol over Alice would introduce a cycle
    ))
    result.lockedPreferences.nodes.map(_.value) should be (candidates)

    result.winner should be (alice)
  }
}

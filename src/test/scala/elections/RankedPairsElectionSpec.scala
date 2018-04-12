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
    "this test" should be ("written")
  }
}

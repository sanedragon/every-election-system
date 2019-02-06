package elections

class ReweightedRankedVotingElectionSpec extends BaseSpec {
  "A ReweightedRankedVotingElection" should "count correctly for a trivial case" in {
    val candidates = Set(alice, bob)

    val election = new ReweightedRankedVotingElection(candidates, 1)

    val ballots = Set(new ScoreBallot(election, Map(alice -> 1, bob -> 0)))

    val result = election.countBallots(ballots)

    result.winners should be (List(alice))
    result.rounds.head.roundResults should be (Map(alice -> 1, bob -> 0))
  }

  it should "count correctly for a slightly less trivial case" in {
    "this test" should be ("written")
  }
}

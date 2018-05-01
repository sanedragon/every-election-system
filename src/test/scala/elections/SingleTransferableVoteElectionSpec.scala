package elections

import elections.SingleTransferableVoteElection.DroopQuota

class SingleTransferableVoteElectionSpec extends BaseSpec {
  "A SingleTransferableVoteElection" should "count correctly for a trivial case" in {
    val election = new SingleTransferableVoteElection(Set(alice, bob), 1, DroopQuota)

    val ballots = Set(new RankedBallot(election, List(alice, bob)))

    val result = election.countBallots(ballots)

    result.rounds.size should be (1)
    result.rounds.head.winners should be (Set(alice))
    result.rounds.head.losers should be (Set.empty)
    result.rounds.head.firstPlaceVotes(alice) should be (1)
    result.rounds.head.firstPlaceVotes.get(bob) should be (None)
  }

  it should "count correctly for a slightly less trivial case" in {
    "this test" should be ("written")
  }
}

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
    val candidates = Set(alice, bob, carol, david)

    val election = new SingleTransferableVoteElection(candidates, 2, DroopQuota)

    // 9 total ballots, 2 positions. Droop Quota is 9/(2 + 1) + 1 = 4
    val ballots = (
      (1 to 4).map(_ => new RankedBallot(election, List(alice, bob, carol, david))) ++
        (1 to 2).map(_ => new RankedBallot(election, List(bob, carol, alice, david))) ++
        (1 to 3).map(_ => new RankedBallot(election, List(carol, alice, bob, david)))
      ).toSet

    val result = election.countBallots(ballots)

    result.rounds.head.firstPlaceVotes should be (Map(alice -> 4.0, bob -> 2.0, carol -> 3.0, david -> 0.0))

    // In the first round, Alice has met the quota with 4 first place votes.
    result.rounds.head.winners should be (Set(alice))

    // No losers because there are winners.
    result.rounds.head.losers should be (Set.empty)

    // In the second round, nobody has met the quota.
    // All of the ballots where alice got first place now have weight 0 because
    // there were exactly enough ballots to meet the quota.
    result.rounds(1).firstPlaceVotes should be (Map(bob -> 2.0, carol -> 3.0, david -> 0.0))
    result.rounds(1).winners should be (Set.empty)
    // David has the lowest first-place vote count (0) and is eliminated
    result.rounds(1).losers should be (Set(david))

    // Eliminating David did not move any first place votes
    result.rounds(2).winners should be (Set.empty)
    // Now Bob has the fewest first place votes (2)
    result.rounds(2).losers should be (Set(bob))

    // Bob's votes get transferred to Carol in the next round
    result.rounds(3).winners should be (Set(carol))
    result.rounds(3).firstPlaceVotes should be (Map(carol -> 5.0))

    result.rounds.size should be (4)

    result.winners should be (Set(alice, carol))
  }

  it should "count correctly for another relatively trivial case" in {
    val candidates = Set(alice, bob, carol, david)

    val election = new SingleTransferableVoteElection(candidates, 2, DroopQuota)

    // 1 total ballots, 2 positions. Droop Quota is floor(11/(2 + 1)) + 1 = 4
    val ballots = (
      (1 to 6).map(_ => new RankedBallot(election, List(alice, bob, carol, david))) ++
        (1 to 2).map(_ => new RankedBallot(election, List(bob, carol, alice, david))) ++
        (1 to 3).map(_ => new RankedBallot(election, List(carol, alice, bob, david)))
      ).toSet

    val result = election.countBallots(ballots)

    result.rounds.head.firstPlaceVotes should be (Map(alice -> 6.0, bob -> 2.0, carol -> 3.0, david -> 0.0))

    // In the first round, Alice has met the quota with 6 first place votes.
    result.rounds.head.winners should be (Set(alice))

    // No losers because there are winners.
    result.rounds.head.losers should be (Set.empty)

    // In the second round, nobody has met the quota.
    // This time, the ballots cast for alice still have weight 1/3 because only 2/3 of them were
    // needed to meet the quota
    result.rounds(1).firstPlaceVotes should be (Map(bob -> (2.0 + 6.0 * (1.0 / 3.0)), carol -> 3.0, david -> 0.0))
    // Bob has met the quota, with 2 first place votes plus 6 1/3-weighted votes from team Alice
    result.rounds(1).winners should be (Set(bob))

    result.rounds.size should be (2)

    result.winners should be (Set(alice, bob))
  }
}

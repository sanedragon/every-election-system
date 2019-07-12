package elections

import elections.SingleTransferableVoteElection._
import org.scalactic.TolerantNumerics

class SingleTransferableVoteElectionSpec extends BaseSpec {
  implicit val closeEnough = TolerantNumerics.tolerantDoubleEquality(0.00000001)
// FIXME: Add test for empty ballot, incomplete ballot, too many candidates eliminated, droop quota not integral
  // diversity requirements fail caused by multiple winners
  "A SingleTransferableVoteElection" should "count correctly for a trivial case" in {
    val election = new SingleTransferableVoteElection(Set(alice, bob), 1, DroopQuota)

    val ballots = Seq(new RankedBallot(List(alice, bob)))

    val result = election.countBallots(ballots)

    result.rounds.size should be (1)
    result.rounds.head.winners should be (Set(alice))
    result.rounds.head.losers should be (Set.empty)
    result.rounds.head.firstPlaceVotes(alice) should be (1)
    result.rounds.head.firstPlaceVotes(bob) should be (0)
  }

  it should "count correctly for a slightly less trivial case" in {
    val candidates = Set(alice, bob, carol, david)

    val election = new SingleTransferableVoteElection(candidates, 2, DroopQuota)

    // 9 total ballots, 2 positions. Droop Quota is 9/(2 + 1) + 1 = 4
    val ballots =
      (1 to 4).map(_ => new RankedBallot(List(alice, bob, carol, david))) ++
        (1 to 2).map(_ => new RankedBallot(List(bob, carol, alice, david))) ++
        (1 to 3).map(_ => new RankedBallot(List(carol, alice, bob, david)))

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

    result.winners should be (List(alice, carol))
  }

  it should "count correctly for another relatively trivial case" in {
    val candidates = Set(alice, bob, carol, david)

    val election = new SingleTransferableVoteElection(candidates, 2, DroopQuota)

    // 1 total ballots, 2 positions. Droop Quota is floor(11/(2 + 1)) + 1 = 4
    val ballots =
      (1 to 6).map(_ => new RankedBallot(List(alice, bob, carol, david))) ++
        (1 to 2).map(_ => new RankedBallot(List(bob, carol, alice, david))) ++
        (1 to 3).map(_ => new RankedBallot(List(carol, alice, bob, david)))

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

    result.winners should be (List(alice, bob))
  }

  it should "count correctly for a another similar case" in {
    val candidates = Set(alice, bob, carol, david)

    val election = new SingleTransferableVoteElection(candidates, 2, DroopQuota)

    // 10 total ballots, 2 positions. Droop Quota is floor(10/(2 + 1)) + 1 = 4
    val ballots =
      (1 to 4).map(_ => new RankedBallot(List(alice, bob, carol, david))) ++
        (1 to 2).map(_ => new RankedBallot(List(bob, carol, alice, david))) ++
        (1 to 4).map(_ => new RankedBallot(List(carol, alice, bob, david)))

    val result = election.countBallots(ballots)

    result.rounds.head.firstPlaceVotes should be (Map(alice -> 4.0, bob -> 2.0, carol -> 4.0, david -> 0.0))

    // In the first round, both Alice and Carol have met the quota with 4 first place votes.
    result.rounds.head.winners should be (Set(alice, carol))

    // No losers because there are winners.
    result.rounds.head.losers should be (Set.empty)

    // That's all!
    result.rounds.size should be (1)

    result.winners should be (List(alice, carol))
  }

  it should "count correctly using the Hare quota" in {
    val candidates = Set(alice, bob, carol, david)

    val election = new SingleTransferableVoteElection(candidates, 2, HareQuota)

    // 9 total ballots, 2 positions. Hare Quota is 9/2 = 4.5
    val ballots = (1 to 4).map(_ => new RankedBallot(List(alice, bob, carol, david))) ++
        (1 to 2).map(_ => new RankedBallot(List(bob, carol, alice, david))) ++
        (1 to 3).map(_ => new RankedBallot(List(carol, alice, bob, david)))

    val result = election.countBallots(ballots)

    result.rounds.head.firstPlaceVotes should be (Map(alice -> 4.0, bob -> 2.0, carol -> 3.0, david -> 0.0))

    // In the first round, nobody has reached the quota.
    result.rounds.head.winners should be (Set.empty)

    // David loses with no first place votes.
    result.rounds.head.losers should be (Set(david))

    // In the second round, nobody has met the quota.
    result.rounds(1).firstPlaceVotes should be (Map(alice -> 4.0, bob -> 2.0, carol -> 3.0))
    result.rounds(1).winners should be (Set.empty)
    // Bob has the lowest first-place vote count (2) and is eliminated
    result.rounds(1).losers should be (Set(bob))

    // Eliminating Bob gave Carol 2 votes, so she has 5 now.
    result.rounds(2).firstPlaceVotes should be (Map(alice -> 4.0, carol -> 5.0))
    result.rounds(2).winners should be (Set(carol))

    // The remaining half-vote after Carol is elected goes to Alice, the last remaining candidate.
    result.rounds(3).firstPlaceVotes(alice) should equal (4.5)
    result.rounds(3).winners should be (Set(alice))

    result.rounds.size should be (4)

    result.winners should be (List(carol, alice))
  }

  it should "handle a tie" in {
    val candidates = Set(alice, bob, carol, david)

    val election = new SingleTransferableVoteElection(candidates, 2, DroopQuota)

    // 12 total ballots, 2 positions. Droop Quota is floor(12/3) + 1 = 5
    val ballots =
      (1 to 4).map(_ => new RankedBallot(List(alice, bob, carol, david))) ++
        (1 to 4).map(_ => new RankedBallot(List(bob, carol, alice, david))) ++
        (1 to 4).map(_ => new RankedBallot(List(carol, alice, bob, david)))

    val result = election.countBallots(ballots)

    result.rounds.head.firstPlaceVotes should be (Map(alice -> 4.0, bob -> 4.0, carol -> 4.0, david -> 0.0))

    // In the first round, nobody has reached the quota.
    result.rounds.head.winners should be (Set.empty)

    // David loses with no first place votes.
    result.rounds.head.losers should be (Set(david))

    // In the second round, nobody has met the quota.
    result.rounds(1).firstPlaceVotes should be (Map(alice -> 4.0, bob -> 4.0, carol -> 4.0))
    result.rounds(1).winners should be (Set.empty)
    // all three candidates tied, so they all lost
    result.rounds(1).losers should be (Set(alice, bob, carol))

    result.rounds.size should be (2)

    // Nobody wins
    result.winners should be (List.empty)
  }

  it should "count correctly with diversity requirements" in {
    val alex = Candidate("Alex", Set("a"))
    val adam = Candidate("Adam", Set("a"))
    val erin = Candidate("Erin", Set("e"))
    val emily = Candidate("Emily", Set("e"))
    val candidates = Set(alex, adam, erin, emily)

    val diversityRequirements = DiversityRequirements(minimums = Map.empty, maximums = Map("a" -> 1))

    val election = new SingleTransferableVoteElection(candidates, 2, DroopQuota, diversityRequirements)

    // 1 total ballots, 2 positions. Droop Quota is floor(11/(2 + 1)) + 1 = 4
    val ballots =
      (1 to 6).map(_ => new RankedBallot(List(alex, adam, erin, emily))) ++
        (1 to 2).map(_ => new RankedBallot(List(adam, erin, alex, emily))) ++
        (1 to 3).map(_ => new RankedBallot(List(erin, alex, adam, emily)))

    val result = election.countBallots(ballots)

    result.rounds.head.firstPlaceVotes should be (Map(alex -> 6.0, adam -> 2.0, erin -> 3.0, emily -> 0.0))

    // In the first round, Alice has met the quota with 6 first place votes.
    result.rounds.head.winners should be (Set(alex))

    // No losers because there are winners.
    result.rounds.head.losers should be (Set.empty)

    // Only one "a" candidate is allowed
    result.rounds.head.diversityExcluded should be (Set(adam))

    result.rounds(1).firstPlaceVotes(erin) should equal (3.0 + 2.0 + 6.0 * (1.0 / 3.0))
    result.rounds(1).firstPlaceVotes(emily) should equal (0.0)

    result.rounds(1).winners should be (Set(erin))

    result.rounds.size should be (2)

    result.winners should be (List(alex, erin))

  }
}
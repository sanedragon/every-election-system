package elections

import org.scalatest._

class PluralityElectionSpec extends FlatSpec with Matchers {

  "A PluralityElection" should "have the correct result for a trivial case" in {
    val alice = Candidate("Alice")
    val bob = Candidate("Bob")

    val election = new PluralityElection(Set(alice, bob))

    val ballots = Set(new SingleVoteBallot(election, alice))

    val result = election.countBallots(ballots)

    result.numVotesByCandidate(alice) should be (1)
    result.numVotesByCandidate(bob) should be (0)

    result.winner should be (alice)
  }

  it should "count correctly for a slightly less trivial case" in {
    "this test" should be ("written")
  }
}
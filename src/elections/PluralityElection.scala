package elections

class PluralityElectionResult(val numVotesByCandidate: Map[Candidate, Int]) extends ElectionResult

class PluralityElection(val candidates: Set[Candidate]) extends Election[SingleVoteBallot, PluralityElectionResult] {
  def countBallots(ballots: Set[SingleVoteBallot]): PluralityElectionResult = {
    val numVotesByCandidate: Map[Candidate, Int] = {
      ballots.foldLeft(Map.empty[Candidate, Int])((tallySoFar: Map[Candidate, Int], ballot: SingleVoteBallot) => {
        tallySoFar.updated(ballot.vote, tallySoFar.getOrElse(ballot.vote, 0) + 1)
      })
    }
    new PluralityElectionResult(numVotesByCandidate)
  }
}


package elections

class SingleTransferableVoteElectionResult(rounds: List[STVRoundResult]) extends ElectionResult

class STVRoundResult(val firstPlaceVotes: Map[Candidate, Int], val winners: Set[Candidate], val losers: Set[Candidate])


abstract class SingleTransferableVoteElection(val candidates: Set[Candidate], val numPositions: Int) extends Election[RankedBallot, SingleTransferableVoteElectionResult] {
  val quotaFromBallotCount: Int => Int

  def countBallots(ballots: Set[RankedBallot]): SingleTransferableVoteElectionResult = {
    val quota = quotaFromBallotCount(ballots.size)

    new SingleTransferableVoteElectionResult(recurseRound(ballots.map(b => WeightedRankedBallot(b,1.0,b.ranking)), quota))
  }

  def recurseRound(
                    ballots: Set[WeightedRankedBallot],
                    quota: Int
                  ): List[STVRoundResult] = {
    val firstPlaceVoteCountByCandidate: Map[Candidate, Int] = ballots.foldLeft(Map.empty[Candidate, Int])((tally, ballot) => {
      val vote = ballot.remainingCandidates.head
      tally.updated(vote, tally.getOrElse(vote, 0) + 1)
    })

    // TODO: Check diversity quotas here. Will also require the already elected candidates passed through recursion.
    val winners: Set[Candidate] = firstPlaceVoteCountByCandidate.foldLeft(Set.empty[Candidate])((winners, p) => {
      val (candidate, count) = p
      if(count > quota) { // TODO: > or >= ?
        winners + candidate
      } else {
        winners
      }
    })

    val losers: Set[Candidate] = if(winners.isEmpty) {
      val lowestVoteCount = firstPlaceVoteCountByCandidate.values.min
      // It's not very likely with a sizeable electorate that more than one candidate loses here
      // But to be fair, if they have the same vote count they all have to lose.
      // This could potentially lead to a tie for last place
      firstPlaceVoteCountByCandidate.foldLeft(Set.empty[Candidate])((losers, p) => {
        val (candidate, count) = p
        if(count == lowestVoteCount) {
          losers + candidate
        } else {
          losers
        }
      })
    } else Set.empty

    val excludedCandidates = winners ++ losers

    if(excludedCandidates.nonEmpty) {

      val winnerQuotaFactors: Map[Candidate, Double] = firstPlaceVoteCountByCandidate.foldLeft(Map.empty[Candidate, Double])((factors, p) => {
        val (candidate, count) = p
        if (winners.contains(candidate)) {
          factors.updated(candidate, (count - quota).toDouble / count)
        } else factors
      })

      val newWeightedBallots: Set[WeightedRankedBallot] = ballots.map(b => {
        WeightedRankedBallot(
          b.originalBallot,
          winnerQuotaFactors.getOrElse(b.remainingCandidates.head, 1.0) * b.weight,
          b.remainingCandidates.filterNot(excludedCandidates.contains)
        )
      })

      new STVRoundResult(firstPlaceVoteCountByCandidate, winners, losers) :: recurseRound(newWeightedBallots, quota)
    } else {
      Nil
    }
  }
}
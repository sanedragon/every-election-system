package elections

class SingleTransferableVoteElectionResult(val rounds: List[STVRoundResult]) extends ElectionResult {
  lazy val winners: Set[Candidate] = rounds.flatMap(r => r.winners).toSet
}

case class STVRoundResult(val firstPlaceVotes: Map[Candidate, Double], val winners: Set[Candidate], val losers: Set[Candidate])

object SingleTransferableVoteElection {
  val DroopQuota: (Int, Int) => Int = (numBallots, numCandidates) => (numBallots / (numCandidates + 1)) + 1
  val HareQuota: (Int, Int) => Int = (numBallots, numCandidates) => (numBallots.toDouble / numCandidates).ceil.toInt
}

class SingleTransferableVoteElection(
                                      val candidates: Set[Candidate],
                                      val numPositions: Int,
                                      val quota: (Int, Int) => Int
                                    ) extends Election[RankedBallot, SingleTransferableVoteElectionResult] {

  def countBallots(ballots: Set[RankedBallot]): SingleTransferableVoteElectionResult = {
    val votesToGetElected = quota(ballots.size, numPositions)

    val initialWeightedBallots = ballots.map(b => WeightedRankedBallot(b,1,b.ranking))

    new SingleTransferableVoteElectionResult(
      recurseRound(
        initialWeightedBallots,
        votesToGetElected,
        numPositions,
        candidates
      )
    )
  }

  def recurseRound(
                    ballots: Set[WeightedRankedBallot],
                    quota: Int,
                    numPositionsRemaining: Int,
                    remainingCandidates: Set[Candidate]
                  ): List[STVRoundResult] = {
    if (numPositionsRemaining == 0 || ballots.isEmpty)
      Nil
    else {
      val zeroTally = remainingCandidates.map(c => c -> 0.0).toMap
      val firstPlaceVoteCountByCandidate: Map[Candidate, Double] =
        ballots.foldLeft(zeroTally)((tally, ballot) => {
          val candidateVotedFor = ballot.remainingVote.head
          tally.updated(candidateVotedFor, tally(candidateVotedFor) + ballot.weight)
        })

      // TODO: Check diversity quotas here. Will also require the already elected candidates passed through recursion.
      val winners: Set[Candidate] = firstPlaceVoteCountByCandidate.foldLeft(Set.empty[Candidate])((winners, p) => {
        val (candidate, count) = p
        if (count >= quota) {
          winners + candidate
        } else {
          winners
        }
      })

      val losers: Set[Candidate] = if (winners.isEmpty) {
        val lowestVoteCount = firstPlaceVoteCountByCandidate.values.min
        // It's not very likely with a sizeable electorate that more than one candidate loses here
        // But to be fair, if they have the same vote count they all have to lose.
        // This could potentially lead to a tie for last place
        firstPlaceVoteCountByCandidate.foldLeft(Set.empty[Candidate])((losers, p) => {
          val (candidate, count) = p
          if (count == lowestVoteCount) {
            losers + candidate
          } else {
            losers
          }
        })
      } else Set.empty

      val excludedCandidates = winners ++ losers

      if (excludedCandidates.nonEmpty) {

        val winnerQuotaFactors: Map[Candidate, Double] =
            firstPlaceVoteCountByCandidate.foldLeft(Map.empty[Candidate, Double])((factors, p) => {
              val (candidate, count) = p
              if (winners.contains(candidate)) {
                factors.updated(candidate, (count - quota) / count)
              } else factors
          })

        val newWeightedBallots: Set[WeightedRankedBallot] = ballots.map(b => {
          WeightedRankedBallot(
            b.originalBallot,
            winnerQuotaFactors.getOrElse(b.remainingVote.head, 1.0) * b.weight,
            b.remainingVote.filterNot(excludedCandidates.contains)
          )
        })

        new STVRoundResult(firstPlaceVoteCountByCandidate, winners, losers) :: recurseRound(newWeightedBallots, quota, numPositionsRemaining - winners.size, remainingCandidates -- excludedCandidates)
      } else {
        Nil
      }
    }
  }
}
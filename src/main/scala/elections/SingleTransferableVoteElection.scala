package elections

class SingleTransferableVoteElectionResult(val rounds: List[STVRoundResult]) extends ElectionResult {
  lazy val winners: Set[Candidate] = rounds.flatMap(r => r.winners).toSet
}

case class STVRoundResult(val firstPlaceVotes: Map[Candidate, Double], val winners: Set[Candidate], val losers: Set[Candidate], val diversityExcludedCandidates: Set[Candidate])

object SingleTransferableVoteElection {
  // Droop Quota is more commonly used, and can be thought of as the smallest number of ballots a candidate can be on
  // where they have a mandate.
  val DroopQuota: (Int, Int) => Double = (numBallots, numCandidates) => ((numBallots / (numCandidates + 1)) + 1).toDouble

  // The Hare Quota can be thought of as the largest number of ballots before you have to select that candidate.
  // Using the Hare Quota tends toward under-representing larger groups.
  val HareQuota: (Int, Int) => Double = (numBallots, numCandidates) => numBallots.toDouble / numCandidates
}

class SingleTransferableVoteElection(
                                      val candidates: Set[Candidate],
                                      val numPositions: Int,
                                      val quota: (Int, Int) => Double = SingleTransferableVoteElection.DroopQuota,
                                      val diversityRequirements: DiversityRequirements = DiversityRequirements.none
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
                    quota: Double,
                    numPositionsRemaining: Int,
                    remainingCandidates: Set[Candidate]
                  ): List[STVRoundResult] = {
    if (numPositionsRemaining == 0 || ballots.isEmpty)
      Nil
    else {
      val diversityExcludedCandidates = diversityRequirements.excludedCandidates(numPositionsRemaining, candidates, remainingCandidates)
      val validCandidates = remainingCandidates -- diversityExcludedCandidates
      val zeroTally = validCandidates.map(c => c -> 0.0).toMap
      val firstPlaceVoteCountByCandidate: Map[Candidate, Double] =
        ballots.foldLeft(zeroTally)((tally, ballot) => {
          val candidateVotedFor = ballot.remainingVote.head
          tally.updated(candidateVotedFor, tally(candidateVotedFor) + ballot.weight)
        })

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

      val eliminatedCandidates = winners ++ losers

      if (eliminatedCandidates == validCandidates) {
        new STVRoundResult(firstPlaceVoteCountByCandidate, winners, losers, diversityExcludedCandidates) :: Nil
      } else if (eliminatedCandidates.nonEmpty) {
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
            b.remainingVote.filterNot(eliminatedCandidates.contains)
          )
        })

        new STVRoundResult(firstPlaceVoteCountByCandidate, winners, losers, diversityExcludedCandidates) ::
          recurseRound(
            newWeightedBallots,
            quota,
            numPositionsRemaining - winners.size,
            validCandidates -- eliminatedCandidates
          )
      } else {
        Nil
      }
    }
  }
}
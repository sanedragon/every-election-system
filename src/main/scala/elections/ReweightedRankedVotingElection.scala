package elections

class RRVElectionResult(val rounds: List[RRVElectionRoundResult]) extends ElectionResult {
  lazy val winners = rounds.map(_.winner)
}

class RRVElectionRoundResult(val roundResults: Map[Candidate, Double], val excludedCandidates: Set[Candidate]) {
  // FIXME: handle ties
  lazy val winner: Candidate = roundResults.maxBy(_._2)._1
}

object ReweightedRankedVotingElection {

  // No vote for a candidate is the same as marking them 0
  val sumScoreAggregation = (sumScore: Double, sumWeight: Double) => sumScore

  // No vote for a candidate does not affect their score, but a candidate could get elected with a tiny fraction of the vote
  val averageScoreAggregation = (sumScore: Double, sumWeight: Double) => sumScore / sumWeight

  // Compromise. Scoring a candidate zero negatively affects them more than not voting for them at all
  val rootWeightScoreAggregation = (sumScore: Double, sumWeight: Double) => sumScore / math.sqrt(sumWeight)

  val defaultScoreAggregation = sumScoreAggregation
  val defaultWeightConstant: Double = 1.0
}


class ReweightedRankedVotingElection(
                                      val candidates: Set[Candidate],
                                      val numPositions: Int,
                                      scoreAggregation: (Double, Double) => Double = ReweightedRankedVotingElection.defaultScoreAggregation,
                                      weightConstant: Double = ReweightedRankedVotingElection.defaultWeightConstant,
                                      val diversityRequirements: DiversityRequirements = DiversityRequirements.none
                                    ) extends Election[ScoreBallot, RRVElectionResult] {

  def countBallots(ballots: Set[ScoreBallot]): RRVElectionResult = {
    val initialWeightedBallots = ballots.map(b => new WeightedScoreBallot(b, 1.0))
    new RRVElectionResult(recurseRound(initialWeightedBallots, Set.empty, numPositions))
  }

  def recurseRound(ballots: Set[WeightedScoreBallot], electedCandidates: Set[Candidate], numPositionsLeft: Int): List[RRVElectionRoundResult] = {
    val diversityExcludedCandidates =
      diversityRequirements.excludedCandidates(
        numPositionsLeft,
        electedCandidates,
        candidates -- electedCandidates
      )

    val remainingCandidates = candidates -- electedCandidates -- diversityExcludedCandidates
    if (numPositionsLeft == 0 || remainingCandidates.isEmpty) Nil
    else {
      val roundResults = remainingCandidates.foldLeft(Map.empty[Candidate, Double])((m, candidate) => {
        val (totalScore: Double, totalWeight: Double) = ballots.foldLeft(0.0, 0.0)((tally, wb) => {
          val (tallyScore, tallyWeight) = tally
          wb.ballot.normalizedScores.get(candidate).map(ballotScore =>
            (tallyScore + (wb.weight * ballotScore), tallyWeight + wb.weight)
          ).getOrElse( (tallyScore, tallyWeight) )
        })
        m + (candidate -> scoreAggregation(totalScore, totalWeight))
      })

      val roundResult = new RRVElectionRoundResult(roundResults, diversityExcludedCandidates)

      val newElectedCandidates = electedCandidates + roundResult.winner
      val reweightedBallots = weightBallots(ballots.map(_.ballot), newElectedCandidates)

      roundResult :: recurseRound(reweightedBallots, newElectedCandidates, numPositionsLeft - 1)
    }
  }
  def weightBallots(ballots: Set[ScoreBallot], electedCandidates: Set[Candidate]): Set[WeightedScoreBallot] = {
    ballots.map(b => {
      val sumOfScoresOfWinners = electedCandidates.foldLeft(0.0)((sum, c) => sum + b.normalizedScores.getOrElse(c,0.0))
      val weight = weightConstant / (weightConstant + sumOfScoresOfWinners)
      WeightedScoreBallot(b, weight)
    })
  }
}
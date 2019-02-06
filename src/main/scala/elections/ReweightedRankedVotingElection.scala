package elections

class RRVElectionResult(val rounds: List[RRVElectionRoundResult]) extends ElectionResult {
  lazy val winners = rounds.map(_.winner)
}

class RRVElectionRoundResult(val roundResults: Map[Candidate, Double]) {
  // FIXME: handle ties
  lazy val winner: Candidate = roundResults.maxBy(_._2)._1
}


class ReweightedRankedVotingElection(val candidates: Set[Candidate], val numPositions: Int) extends Election[ScoreBallot, RRVElectionResult] {
  def countBallots(ballots: Set[ScoreBallot]): RRVElectionResult = {
    new RRVElectionResult(recurseRound(ballots, Set.empty, numPositions))
  }

  def recurseRound(ballots: Set[ScoreBallot], electedCandidates: Set[Candidate], numPositionsLeft: Int): List[RRVElectionRoundResult] = {
    val candidatesNotYetElected = candidates -- electedCandidates
    if (numPositionsLeft == 0 || candidatesNotYetElected.isEmpty) Nil
    else {
      val weightedBallots = weightBallots(ballots, electedCandidates)
      val roundResult = calculateRoundResult(weightedBallots, candidatesNotYetElected)
      roundResult :: recurseRound(ballots, electedCandidates + roundResult.winner, numPositionsLeft - 1)
    }
  }

  def calculateRoundResult(ballots: Set[WeightedScoreBallot], candidates: Set[Candidate]): RRVElectionRoundResult = {
    val roundResults = candidates.foldLeft(Map.empty[Candidate, Double])((m, candidate) => {
      val (totalScore: Double, totalWeight: Double) = ballots.foldLeft(0.0, 0.0)((tally, wb) => {
        val (tallyScore, tallyWeight) = tally
        wb.ballot.normalizedScores.get(candidate).map(ballotScore =>
          (tallyScore + ballotScore, tallyWeight + wb.weight)
        ).getOrElse( (tallyScore, tallyWeight) )
      })
      val averageScore = totalScore / totalWeight
      m + (candidate -> averageScore)
    })
    new RRVElectionRoundResult(roundResults)
  }

  val weightConstant: Double = 1

  def weightBallots(ballots: Set[ScoreBallot], electedCandidates: Set[Candidate]): Set[WeightedScoreBallot] = {
    ballots.map(b => {
      val sumOfScoresOfWinners = electedCandidates.foldLeft(0.0)((sum, c) => sum + b.normalizedScores.getOrElse(c,0.0))
      val weight = weightConstant / (weightConstant + sumOfScoresOfWinners)
      WeightedScoreBallot(b, weight)
    })
  }
}
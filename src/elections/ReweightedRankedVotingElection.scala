package elections

import spire.math.Rational

class RRVElectionResult(val rounds: List[RRVElectionRoundResult]) extends ElectionResult {
  lazy val winners = rounds.map(_.winner)
}

class RRVElectionRoundResult(val roundResults: Map[Candidate, Rational]) {
  lazy val winner: Candidate = roundResults.maxBy(_._2)._1
}


class ReweightedRankedVotingElection(val candidates: Set[Candidate]) extends Election[ScoreBallot, RRVElectionResult] {
  def countBallots(ballots: Set[ScoreBallot]): RRVElectionResult = {
    new RRVElectionResult(recurseRound(ballots, Set.empty))
  }

  def recurseRound(ballots: Set[ScoreBallot], electedCandidates: Set[Candidate]): List[RRVElectionRoundResult] = {
    val candidatesNotYetElected = candidates -- electedCandidates
    if(candidatesNotYetElected.nonEmpty) {
      val weightedBallots = weightBallots(ballots, electedCandidates)
      val roundResult = calculateRoundResult(weightedBallots, candidatesNotYetElected)
      roundResult :: recurseRound(ballots, electedCandidates + roundResult.winner)
    } else {
      Nil
    }
  }

  def calculateRoundResult(ballots: Set[WeightedScoreBallot], candidates: Set[Candidate]): RRVElectionRoundResult = {
    val roundResults = candidates.foldLeft(Map.empty[Candidate, Rational])((m, candidate) => {
      val (totalScore: Rational, totalWeight: Rational) = ballots.foldLeft(Rational(0), Rational(0))((tally, wb) => {
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

  val weightConstant = Rational(1)

  def weightBallots(ballots: Set[ScoreBallot], electedCandidates: Set[Candidate]): Set[WeightedScoreBallot] = {
    ballots.map(b => {
      val sumOfScoresOfWinners = electedCandidates.map(b.normalizedScores.getOrElse(_,Rational(0))).sum
      val weight = weightConstant / (weightConstant + sumOfScoresOfWinners)
      WeightedScoreBallot(b, weight)
    })
  }
}
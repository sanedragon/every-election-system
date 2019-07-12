package elections

import scala.collection.mutable

case class PlainScoreElectionResult(scores: Map[Candidate, Double]) extends ElectionResult

class PlainScoreElection(val candidates: Set[Candidate]) extends Election[ScoreBallot, PlainScoreElectionResult] {
  override def countBallots(ballots: Seq[ScoreBallot]): PlainScoreElectionResult = {
    val tally = mutable.Map.empty[Candidate, Double].withDefaultValue(0.0)
    ballots.foreach(ballot => {
      candidates.foreach(candidate => {
        tally(candidate) += ballot.normalizedScores.getOrElse(candidate, 0.0)
      })
    })
    PlainScoreElectionResult(tally.toMap)
  }
}

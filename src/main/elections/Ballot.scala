package elections

import spire.math.Rational

abstract class Ballot {
  val election: Election
  def validate: Boolean
}

case class ScoreBallot(val election: Election, val scores: Map[Candidate, Int]) extends Ballot {
  def validate = {
    scores.keySet.forall(candidate => election.candidates.contains(candidate)) &&
    scores.values.forall(score => score >= 0)
  }

  lazy val normalizedScores: Map[Candidate, Rational] = {
    val maxScore = scores.values.max
    scores.mapValues(score => Rational(score, maxScore))
  }
}

case class RankedBallot(val election: Election, val ranking: List[Candidate]) extends Ballot {
  def validate = {
    ranking.toSet.size == ranking.size &&
      ranking.nonEmpty &&
      ranking.forall(candidate => election.candidates.contains(candidate))
  }
}

case class ApprovalBallot(val election: Election, val approvals: Map[Candidate, Boolean]) extends Ballot {
  def validate = {
    approvals.keySet.forall(candidate => election.candidates.contains(candidate))
  }
}

case class SingleVoteBallot(val election: Election, val vote: Candidate) extends Ballot {
  def validate = {
    election.candidates.contains(vote)
  }
}

case class WeightedRankedBallot(originalBallot: RankedBallot, weight: Double, remainingCandidates: List[Candidate])
case class WeightedScoreBallot(ballot: ScoreBallot, weight: Rational)

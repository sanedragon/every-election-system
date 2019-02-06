package elections

import java.util.UUID

abstract class Ballot {
  final val id = UUID.randomUUID()
  val election: Election[_,_]
  def validate: Boolean

  final override def equals(other: Any) = other.isInstanceOf[Ballot] && other.asInstanceOf[Ballot].id == id
  final override lazy val hashCode = election.hashCode() * 7 + id.hashCode()
}

class ScoreBallot(
                    val election: Election[_ <: ScoreBallot, _ <: ElectionResult],
                    val scores: Map[Candidate, Int]
                  ) extends Ballot {
  def validate = {
    scores.keySet.forall(candidate => election.candidates.contains(candidate)) &&
    scores.values.forall(score => score >= 0)
  }

  lazy val normalizedScores: Map[Candidate, Double] = {
    val maxScore = scores.values.max.toDouble
    scores.mapValues(score => score / maxScore)
  }
}

class RankedBallot(
                     val election: Election[_ <: RankedBallot, _ <: ElectionResult],
                     val ranking: List[Candidate]
                   ) extends Ballot {
  def validate = {
    ranking.toSet.size == ranking.size &&
      ranking.nonEmpty &&
      ranking.forall(candidate => election.candidates.contains(candidate))
  }
}

class ApprovalBallot(
                       val election: Election[_ <: ApprovalBallot, _ <: ElectionResult],
                       val approvals: Map[Candidate, Boolean]
                    ) extends Ballot {
  def validate = {
    approvals.keySet.forall(candidate => election.candidates.contains(candidate))
  }
}

class SingleVoteBallot(
                         val election: Election[_ <: SingleVoteBallot, _ <: ElectionResult],
                         val vote: Candidate
                       ) extends Ballot {
  def validate = {
    election.candidates.contains(vote)
  }
}

case class WeightedRankedBallot(originalBallot: RankedBallot, weight: Double, remainingVote: List[Candidate])
case class WeightedScoreBallot(ballot: ScoreBallot, weight: Double)


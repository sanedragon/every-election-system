package elections

class ElectionResult

case class Candidate(name: String) {
  override val toString = name
}

abstract class Election[BallotT<:Ballot, ElectionResultT<:ElectionResult] {
  val candidates: Set[Candidate]
  def countBallots(ballots: Set[BallotT]): ElectionResultT
}
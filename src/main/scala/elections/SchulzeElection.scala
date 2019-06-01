package elections

import scalax.collection.Graph

case class SchulzeElectionResult(preferenceMatrix: PreferenceMatrix, rounds: List[SchulzeEliminationRound]) extends ElectionResult

class RankedSchulzeElection(val candidates: Set[Candidate]) extends SchulzeElection[RankedBallot] {
  def calculatePreferenceMatrix(ballots: Seq[RankedBallot]) = PreferenceMatrix.fromRankedBallots(candidates, ballots)
}

class ScoreSchulzeElection(val candidates: Set[Candidate]) extends SchulzeElection[ScoreBallot] {
  def calculatePreferenceMatrix(ballots: Seq[ScoreBallot]) = PreferenceMatrix.fromScoreBallots(candidates, ballots)
}

case class SchulzeEliminationRound(val schwartzSet: Set[Candidate], prunedPreferences: List[Preference[Candidate]])

abstract class SchulzeElection[BallotT<:Ballot] extends Election[BallotT, SchulzeElectionResult] {
  def countBallots(ballots: Seq[BallotT]): SchulzeElectionResult = {
    val preferenceMatrix = calculatePreferenceMatrix(ballots)
    val preferences = preferenceMatrix.preferencesSmallestFirst.toList
    SchulzeElectionResult(preferenceMatrix, recurseRound(candidates, preferences))
  }

  def recurseRound(remainingCandidates: Set[Candidate], preferences: List[Preference[Candidate]]): List[SchulzeEliminationRound] = {
    if(preferences.isEmpty) Nil
    else {
      val preferenceGraph: Graph[Candidate, Preference] = Graph.from(remainingCandidates, preferences)
      val schwartzSet = PreferenceMatrix.calculateSchwartzSet(preferenceGraph)

      if(schwartzSet != remainingCandidates) {
        val schwartzFilteredPreferences = preferences.filter(
          p => schwartzSet.contains(p.yes) && schwartzSet.contains(p.no)
        )
        SchulzeEliminationRound(schwartzSet, schwartzFilteredPreferences) :: recurseRound(schwartzSet, schwartzFilteredPreferences)
      } else {
        SchulzeEliminationRound(schwartzSet, preferences.tail) :: recurseRound(schwartzSet, preferences.tail)
      }


    }
  }


  def calculatePreferenceMatrix(ballots: Seq[BallotT]): PreferenceMatrix

}

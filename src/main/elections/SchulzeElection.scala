package elections

import scalax.collection.Graph

case class SchulzeElectionResult(preferenceMatrix: PreferenceMatrix, rounds: List[SchulzeEliminationRound]) extends ElectionResult

class RankedSchulzeElection(val candidates: Set[Candidate]) extends SchulzeElection[RankedBallot] {
  def calculatePreferenceMatrix(ballots: Set[RankedBallot]) = PreferenceMatrix.fromRankedBallots(candidates, ballots)
}

case class SchulzeEliminationRound(val schwartzSet: Set[Candidate], prunedPreferenceGraph: List[Preference[Candidate]])

abstract class SchulzeElection[BallotT<:Ballot] extends Election[BallotT, SchulzeElectionResult] {
  def countBallots(ballots: Set[BallotT]): SchulzeElectionResult = {
    val preferenceMatrix = calculatePreferenceMatrix(ballots)
    val preferences = preferenceMatrix.preferencesSmallestFirst.toList
    SchulzeElectionResult(preferenceMatrix, recurseRound(candidates, preferences))
  }

  def recurseRound(remainingCandidates: Set[Candidate], preferences: List[Preference[Candidate]]): List[SchulzeEliminationRound] = {
    if(preferences.isEmpty) Nil
    else {
      val preferenceGraph: Graph[Candidate, Preference] = Graph.from(candidates, preferences)
      val schwartzSet = calculateSchwartzSet(remainingCandidates, preferenceGraph)

      val limitedPreferences = preferences.filter(
        p => schwartzSet.contains(p.yes) && schwartzSet.contains(p.no)
      ).tail

      SchulzeEliminationRound(schwartzSet, limitedPreferences) :: recurseRound(schwartzSet, limitedPreferences)
    }
  }

  // Floyd-Warshall isn't the most efficient algorithm for this but it is easy to implement
  def calculateSchwartzSet(candidates: Set[Candidate], preferenceGraph: Graph[Candidate, Preference]): Set[Candidate] = {
    for(
      x <- candidates;
      y <- candidates
        if preferenceGraph.get(x).pathTo(preferenceGraph.get(y)).isEmpty &&
           preferenceGraph.get(y).pathTo(preferenceGraph.get(x)).nonEmpty
    ) yield x
  }

  def calculatePreferenceMatrix(ballots: Set[BallotT]): PreferenceMatrix

}

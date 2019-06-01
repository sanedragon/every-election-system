package elections

import scalax.collection.Graph

class RankedPairsElectionResult(
                                    val preferenceMatrix: PreferenceMatrix,
                                    val strongestPreferences: Seq[Preference[Candidate]],
                                    val lockedPreferences: Graph[Candidate, Preference],
                                    val winner: Candidate
                                  ) extends ElectionResult

class RankedBallotRankedPairsElection(val candidates: Set[Candidate]) extends RankedPairsElection[RankedBallot] {
  def calculatePreferenceMatrix(ballots: Seq[RankedBallot]) = PreferenceMatrix.fromRankedBallots(candidates, ballots)
}

class ScoreBallotRankedPairsElection(val candidates: Set[Candidate]) extends RankedPairsElection[ScoreBallot] {
  def calculatePreferenceMatrix(ballots: Seq[ScoreBallot]) = PreferenceMatrix.fromScoreBallots(candidates, ballots)
}

abstract class RankedPairsElection[BallotT <: Ballot] extends Election[BallotT, RankedPairsElectionResult] {
  def countBallots(ballots: Seq[BallotT]): RankedPairsElectionResult = {
    val preferenceMatrix = calculatePreferenceMatrix(ballots)
    val strongestPreferences: Seq[Preference[Candidate]] = preferenceMatrix.preferencesLargestFirst

    val acyclicPreferences: List[Preference[Candidate]] =
      strongestPreferences.foldLeft(List.empty[Preference[Candidate]])(
        (soFar: List[Preference[Candidate]], p: Preference[Candidate]) => {
          if(Graph.from(candidates, p :: soFar).isCyclic) {
            soFar
          } else {
            p :: soFar
          }
      })

    val acyclicPreferenceGraph: Graph[Candidate, Preference] = Graph.from[Candidate, Preference](candidates, acyclicPreferences)

    val winner = acyclicPreferenceGraph.get(candidates.head)
      .pathUntil(candidate => !candidate.hasSuccessors).map(_.endNode.value) // Follow the preference graph
      .getOrElse(candidates.head) // Or we started at the winner

    new RankedPairsElectionResult(preferenceMatrix, strongestPreferences, acyclicPreferenceGraph, winner)
  }

  def calculatePreferenceMatrix(ballots: Seq[BallotT]): PreferenceMatrix
}
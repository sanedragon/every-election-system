package elections

import scalax.collection.Graph

class RankedPairsElectionResult(
                                    val preferenceMatrix: PreferenceMatrix,
                                    val strongestPreferences: Seq[Preference[Candidate]],
                                    val lockedPreferences: Graph[Candidate, Preference],
                                    val orderedCandidates: Seq[(Candidate, Double)]
                                  ) extends ElectionResult {
  val winner = orderedCandidates.head
}

class RankedBallotRankedPairsElection(val candidates: Set[Candidate], reverse: Boolean = false) extends RankedPairsElection[RankedBallot] {
  def calculatePreferenceMatrix(ballots: Seq[RankedBallot]) = PreferenceMatrix.fromRankedBallots(candidates, ballots, reverse)
}

class ScoreBallotRankedPairsElection(val candidates: Set[Candidate]) extends RankedPairsElection[ScoreBallot] {
  def calculatePreferenceMatrix(ballots: Seq[ScoreBallot]) = PreferenceMatrix.fromScoreBallots(candidates, ballots)
}

trait RankedPairsElection[BallotT <: Ballot] extends Election[BallotT, RankedPairsElectionResult] {
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

    val preliminaryWinner = acyclicPreferenceGraph.get(candidates.head)
      .pathUntil(candidate => !candidate.hasSuccessors).map(_.endNode.value) // Follow the preference graph
      .getOrElse(candidates.head) // Or we started at the winner

    val orderedCandidates = {
      // Candidates are ordered by the distance from the winner on the full preference graph, including ties
      val winnerNode = preferenceMatrix.graphIncludingTies.get(preliminaryWinner)
      candidates.map(candidate => {
        val shortestPathWeight = preferenceMatrix.graphIncludingTies.get(candidate)
          .shortestPathTo(winnerNode).map(_.weight).getOrElse(Double.PositiveInfinity)
        candidate -> shortestPathWeight
      }).toSeq.sortBy(_._2)
    }

    new RankedPairsElectionResult(preferenceMatrix, strongestPreferences, acyclicPreferenceGraph, orderedCandidates)
  }

  def calculatePreferenceMatrix(ballots: Seq[BallotT]): PreferenceMatrix
}
package elections

import spire.math.Rational

import scala.collection.mutable
import scalax.collection.edge.WUnDiEdge

class PreferenceMatrix(val pairwisePreferences: Map[(Candidate, Candidate), Double]) {
  lazy val positivePreferences: Seq[Preference[Candidate]] = pairwisePreferences.toSeq.flatMap((p) => {
    val ((x,y), strength) = p
    if (strength > 0) {
      Some(Preference(x, y, strength))
    } else {
      None
    }
  })

  lazy val preferencesSmallestFirst = positivePreferences.sortBy(_.strength)

  lazy val preferencesLargestFirst = preferencesSmallestFirst.reverse
}

case class Preference[T](yes: T, no: T, strength: Double)
  extends WUnDiEdge[T]((yes, no), strength)

object PreferenceMatrix {
  def fromRankedBallots(candidates: Set[Candidate], ballots: Set[RankedBallot]): PreferenceMatrix = {
    val tally = mutable.Map.empty[(Candidate, Candidate), Int]

    ballots.foreach(ballot => {
      candidates.foreach(x => {candidates.foreach(y => {
        if(x != y) {
          // the preferred candidate is the one that appears first in the ranking.
          // If neither candidate appears, do nothing
          val preferredCandidate: Option[Candidate] = ballot.ranking.find(candidate => candidate == x || candidate == y)
          preferredCandidate.foreach(c => {
            if(c == x) {
              tally += ((x,y) -> (tally.getOrElse((x,y), 0) + 1))
            } else {
              tally += ((x,y) -> (tally.getOrElse((x,y), 0) - 1))
            }
          })
        }
      })})
    })

    new PreferenceMatrix(tally.toMap.map(_ match {
      case (candidatePair: (Candidate, Candidate), strength: Int) => candidatePair -> strength.toDouble
    }))
  }

  // TODO: Offer other options for counting scored ballots into a preference matrix
  // Various choices include the treatment of unrated candidates, whether the difference in score is reduced to
  // (-1, 0, 1) like the ranked ballot system is.
  def fromScoreBallots(candidates: Set[Candidate], ballots: Set[ScoreBallot]): PreferenceMatrix = {
    val tally = mutable.Map.empty[(Candidate, Candidate), Rational]

    ballots.foreach(ballot => {
      candidates.foreach(x => {candidates.foreach(y => {
        if(x != y) {
          val preference: Option[Rational] = (ballot.normalizedScores.get(x), ballot.normalizedScores.get(y)) match {
            case (_, None)                    => None
            case (None, _)                    => None
            case (Some(scoreX), Some(scoreY)) => Some(scoreX - scoreY)
          }
          preference.foreach(p => {
            tally += ((x,y) -> (tally.getOrElse((x,y), Rational(0)) + p))
          })
        }
      })})
    })

    new PreferenceMatrix(tally.toMap.map(_ match {
      case (candidatePair: (Candidate, Candidate), strength: Rational) => candidatePair -> strength.toDouble
    }))
  }
}
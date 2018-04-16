package elections

import spire.math.Rational

import scala.collection.mutable
import scalax.collection.edge._

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
  extends WDiEdge[T]((no, yes), strength) {

  override lazy val toString: String = yes.toString + " over " + no.toString + " at " + strength
  override def equals(other: Any) = other match {
    case Preference(y, n, s) => y == yes && n == no && s == strength
    case _ => false
  }
}

object PreferenceMatrix {
  def fromRankedBallots(candidates: Set[Candidate], ballots: Set[RankedBallot]): PreferenceMatrix = {
    val tally = mutable.Map.empty[(Candidate, Candidate), Int]
    for(
      x <- candidates;
      y <- candidates if x != y
    ) {
      tally += ((x,y) -> 0)
    }

    ballots.foreach(ballot => {
      candidates.foreach(x => {candidates.foreach(y => {
        if(x != y) {
          // the preferred candidate is the one that appears first in the ranking.
          // If neither candidate appears, do nothing
          val preferredCandidate: Option[Candidate] = ballot.ranking.find(candidate => candidate == x || candidate == y)
          preferredCandidate.foreach(c => {
            if(c == x) {
              tally((x,y)) += 1
            } else {
              tally((x,y)) -= 1
            }
          })
        }
      })})
    })

    new PreferenceMatrix(tally.toMap.map(_ match {
      case (candidatePair: (Candidate, Candidate), strength: Int) => candidatePair -> strength.toDouble
    }))
  }

  val normalizedScoresToPreferenceUsingDifference: (Option[Rational], Option[Rational]) => Option[Rational] =
    (xScore, yScore) => {
      (xScore, yScore) match {
          case (Some(scoreX), Some(scoreY)) => Some(scoreX - scoreY)
          case _ => None
        }
    }

  val normalizedScoresToPreferenceIntegral: (Option[Rational], Option[Rational]) => Option[Rational] =
    (xScore, yScore) => {
      (xScore, yScore) match {
          case (Some(scoreX), Some(scoreY)) if scoreX > scoreY => Some(1)
          case (Some(scoreX), Some(scoreY)) if scoreX < scoreY => Some(-1)
          case _ => None
        }
    }

  def fromScoreBallots(candidates: Set[Candidate], ballots: Set[ScoreBallot]): PreferenceMatrix = {
    val tally = mutable.Map.empty[(Candidate, Candidate), Rational]
    for(
      x <- candidates;
      y <- candidates if x != y
    ) {
      tally += ((x,y) -> 0)
    }

    // TODO: Allow this to be changed
    val preferenceCalc: (Option[Rational], Option[Rational]) => Option[Rational] =
      normalizedScoresToPreferenceUsingDifference

    ballots.foreach(ballot => {
      candidates.foreach(x => {candidates.foreach(y => {
        if(x != y) {
          preferenceCalc(ballot.normalizedScores.get(x), ballot.normalizedScores.get(y)).foreach(tally(x,y) += _)
        }
      })})
    })


    new PreferenceMatrix(tally.toMap.map(_ match {
      case (candidatePair: (Candidate, Candidate), strength: Rational) => candidatePair -> strength.toDouble
    }))
  }
}
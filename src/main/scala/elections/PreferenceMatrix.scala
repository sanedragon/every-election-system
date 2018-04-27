package elections

import spire.math.Rational

import scala.collection.mutable
import scalax.collection.Graph
import scalax.collection.edge._

/*
 pairwisePreferences(alice, bob): the number of ballots ranking alice over bob, or equivalent
 */
class PreferenceMatrix(val candidates: Set[Candidate], val pairwisePreferences: Map[(Candidate, Candidate), Double]) {

  lazy val candidatePairs = for(
    x <- candidates;
    y <- candidates if x != y
  ) yield (x,y)

  lazy val directionalPreferences = candidatePairs.flatMap(p => {
    val strength = pairwisePreferences(p)
    val reverseStrength = pairwisePreferences((p._2, p._1))
    if (strength > reverseStrength) {
      Some(Preference(p._1, p._2, strength - reverseStrength))
    } else {
      None
    }
  })

  lazy val graph = Graph.from(candidates, directionalPreferences)

  lazy val graphIncludingTies = Graph.from(candidates, nonNegativePreferences)

  lazy val nonNegativePreferences = for(
    x <- candidates;
    y <- candidates if x != y && pairwisePreferences((x, y)) >= pairwisePreferences((y, x))
  ) yield Preference(x,y, pairwisePreferences(x,y) - pairwisePreferences(y,x))

  lazy val preferencesSmallestFirst = directionalPreferences.toSeq.sortBy(_.strength)

  lazy val preferencesLargestFirst = preferencesSmallestFirst.reverse

  lazy val summedPreferenceGraph = Graph.from(candidates, directionalPreferences)

  lazy val schwartzSet = PreferenceMatrix.calculateSchwartzSet(graph)
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
    val tally = mutable.Map.empty[(Candidate, Candidate), Int].withDefaultValue(0)

    ballots.foreach(ballot => {
      candidates.foreach(x => {candidates.foreach(y => {
        if(x != y) {
          // the preferred candidate is the one that appears first in the ranking.
          // If neither candidate appears, do nothing
          val preferredCandidate: Option[Candidate] = ballot.ranking.find(candidate => candidate == x || candidate == y)
          preferredCandidate.foreach(c => {
            if(c == x) {
              tally((x,y)) += 1
            }
          })
        }
      })})
    })

    new PreferenceMatrix(candidates, tally.toMap.map(_ match {
      case (candidatePair: (Candidate, Candidate), strength: Int) => candidatePair -> strength.toDouble
    }).withDefaultValue(0))
  }

  val normalizedScoresToPreferenceUsingDifference: (Option[Rational], Option[Rational]) => Rational =
    (xScore, yScore) => {
      (xScore, yScore) match {
          case (Some(scoreX), Some(scoreY)) if scoreX > scoreY => scoreX - scoreY
          case _                            => 0
        }
    }

  val normalizedScoresToPreferenceIntegral: (Option[Rational], Option[Rational]) => Rational =
    (xScore, yScore) => {
      (xScore, yScore) match {
          case (Some(scoreX), Some(scoreY)) if scoreX > scoreY => 1
          case _                                               => 0
        }
    }



  def fromScoreBallots(candidates: Set[Candidate], ballots: Set[ScoreBallot]): PreferenceMatrix = {
    val tally = mutable.Map.empty[(Candidate, Candidate), Rational].withDefaultValue(0)

    // TODO: Allow this to be changed
    val preferenceCalc: (Option[Rational], Option[Rational]) => Rational =
      normalizedScoresToPreferenceUsingDifference

    ballots.foreach(ballot => {
      candidates.foreach(x => {candidates.foreach(y => {
        if(x != y) {
          tally(x,y) += preferenceCalc(ballot.normalizedScores.get(x), ballot.normalizedScores.get(y))
        }
      })})
    })


    new PreferenceMatrix(candidates, tally.toMap.map(_ match {
      case (candidatePair: (Candidate, Candidate), strength: Rational) => candidatePair -> strength.toDouble
    }).withDefaultValue(0))
  }

  // Floyd-Warshall isn't the most efficient algorithm for this but it is easy to implement
  def calculateSchwartzSet(preferenceGraph: Graph[Candidate, Preference]): Set[Candidate] = {
    val candidates = preferenceGraph.nodes.map(_.value).toSet
    val schwartzSet = for(
      x <- candidates;
      y <- candidates
      if preferenceGraph.get(x).pathTo(preferenceGraph.get(y)).isEmpty &&
        preferenceGraph.get(y).pathTo(preferenceGraph.get(x)).nonEmpty
    ) yield x
    if(schwartzSet.nonEmpty)
      schwartzSet
    else
      candidates
  }

}
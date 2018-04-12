package elections

class PreferenceMatrixSpec extends BaseSpec {
  "A PreferenceMatrix" should "tabulate a single ranked ballot correctly" in {
    val candidates = Set(alice, bob, carol)
    val election = new RankedBallotRankedPairsElection(candidates)

    val ballots = Set(new RankedBallot(election, List(alice, bob, carol)))

    val preferenceMatrix = PreferenceMatrix.fromRankedBallots(candidates, ballots)

    preferenceMatrix.pairwisePreferences((alice, bob)) should be (1)
    preferenceMatrix.pairwisePreferences((alice, carol)) should be (1)
    preferenceMatrix.pairwisePreferences((bob, carol)) should be (1)
    preferenceMatrix.pairwisePreferences((carol, alice)) should be (-1)
    preferenceMatrix.pairwisePreferences((carol, bob)) should be (-1)
    preferenceMatrix.pairwisePreferences((bob, alice)) should be (-1)
  }

  it should "tabulate a small set of ranked ballots correctly" in {
    val candidates = Set(alice, bob, carol)
    val election = new RankedBallotRankedPairsElection(candidates)

    val ballots = Set(
      new RankedBallot(election, List(alice, bob, carol)),
      new RankedBallot(election, List(bob, alice, carol)),
      new RankedBallot(election, List(alice, carol, bob))
    )

    val preferenceMatrix = PreferenceMatrix.fromRankedBallots(candidates, ballots)

    preferenceMatrix.pairwisePreferences((alice, bob)) should be (1)
    preferenceMatrix.pairwisePreferences((alice, carol)) should be (3)
    preferenceMatrix.pairwisePreferences((bob, carol)) should be (1)
    preferenceMatrix.pairwisePreferences((carol, alice)) should be (-3)
    preferenceMatrix.pairwisePreferences((carol, bob)) should be (-1)
    preferenceMatrix.pairwisePreferences((bob, alice)) should be (-1)
  }

  it should "tabulate a single score ballot correctly" in {
    val candidates = Set(alice, bob, carol, david)
    val election = new ScoreBallotRankedPairsElection(candidates)

    val ballots = Set(
      new ScoreBallot(election, Map(alice -> 0, bob -> 2, carol -> 2))
    )

    val preferenceMatrix = PreferenceMatrix.fromScoreBallots(candidates, ballots)

    preferenceMatrix.pairwisePreferences((alice, bob)) should be (-1)
    preferenceMatrix.pairwisePreferences((alice, carol)) should be (-1)
    preferenceMatrix.pairwisePreferences((alice, david)) should be (0)
    preferenceMatrix.pairwisePreferences((bob, alice)) should be (1)
    preferenceMatrix.pairwisePreferences((bob, carol)) should be (0)
    preferenceMatrix.pairwisePreferences((bob, david)) should be (0)
    preferenceMatrix.pairwisePreferences((carol, alice)) should be (1)
    preferenceMatrix.pairwisePreferences((carol, bob)) should be (0)
    preferenceMatrix.pairwisePreferences((carol, david)) should be (0)
    preferenceMatrix.pairwisePreferences((david, alice)) should be (0)
    preferenceMatrix.pairwisePreferences((david, bob)) should be (0)
    preferenceMatrix.pairwisePreferences((david, carol)) should be (0)
  }

  it should "tabulate a small set of score ballots correctly" in {
    val candidates = Set(alice, bob, carol, david)
    val election = new ScoreBallotRankedPairsElection(candidates)

    val ballots = Set(
      new ScoreBallot(election, Map(alice -> 0, bob -> 2, carol -> 2)),
      new ScoreBallot(election, Map(alice -> 1, bob -> 2, carol -> 10, david -> 5))
    )

    val preferenceMatrix = PreferenceMatrix.fromScoreBallots(candidates, ballots)

    preferenceMatrix.pairwisePreferences((alice, bob)) should be (-1 + -0.1)
    preferenceMatrix.pairwisePreferences((alice, carol)) should be (-1 + -0.9)
    preferenceMatrix.pairwisePreferences((alice, david)) should be (0 + -0.4)
    preferenceMatrix.pairwisePreferences((bob, alice)) should be (1 + 0.1)
    preferenceMatrix.pairwisePreferences((bob, carol)) should be (0 + -0.8)
    preferenceMatrix.pairwisePreferences((bob, david)) should be (0 + -0.3)
    preferenceMatrix.pairwisePreferences((carol, alice)) should be (1 + 0.9)
    preferenceMatrix.pairwisePreferences((carol, bob)) should be (0 + 0.8)
    preferenceMatrix.pairwisePreferences((carol, david)) should be (0 + 0.5)
    preferenceMatrix.pairwisePreferences((david, alice)) should be (0 + 0.4)
    preferenceMatrix.pairwisePreferences((david, bob)) should be (0 + 0.3)
    preferenceMatrix.pairwisePreferences((david, carol)) should be (0 + -0.5)
  }

  it should "get the positive preferences correctly" in {
    "this test" should be ("written")
  }
}

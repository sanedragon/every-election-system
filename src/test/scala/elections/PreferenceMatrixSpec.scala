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

  it should "get tabulated correctly from a trivial set of scored ballots" in {
    "this test" should be ("written")
  }

  it should "get the positive preferences correctly" in {
    "this test" should be ("written")
  }
}

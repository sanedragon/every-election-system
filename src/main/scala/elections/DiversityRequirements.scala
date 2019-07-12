package elections

object DiversityRequirements {
  val none = new DiversityRequirements(Map.empty, Map.empty)
}

case class DiversityRequirements(
                             minimums: Map[String, Int],
                             maximums: Map[String, Int]
                           ) {
  // at least 5 must be racial or national minorities
  // no more than 8 may be men
  val categories: Set[String] = minimums.keySet ++ maximums.keySet

  def protectedCandidates(
                           numRemainingPositions: Int,
                           alreadyElectedCandidates: Set[Candidate],
                           remainingCandidates: Set[Candidate]
                         ): Set[Candidate] = {
    val numCandidatesElectedByCategory: Map[String, Int] = categories.map(category =>
      category -> alreadyElectedCandidates.count(candidate => candidate.diversityCategories.contains(category))
    ).toMap

    val remainingCandidatesTowardsMinimums: Map[String, Int] = minimums.keys.map(category =>
      category -> remainingCandidates.count(candidate => candidate.diversityCategories.contains(category))
    ).toMap

    val remainingCandidatesWithoutCategory: Map[String, Int] = maximums.keys.map(category =>
      category -> remainingCandidates.count(candidate => !candidate.diversityCategories.contains(category))
    ).toMap

    val protectedCategories = minimums.keys.filter(category => {
      minimums.get(category).exists(minimum =>
        numCandidatesElectedByCategory(category) + remainingCandidatesTowardsMinimums(category) <= minimum
      )
    })

    val protectedAntiCategories = maximums.keys.filter(category => {
      maximums.get(category).exists(maximum => {
        val numStillNeededWithoutCategory = numRemainingPositions - (maximum - numCandidatesElectedByCategory(category))
        remainingCandidatesWithoutCategory(category) <= numStillNeededWithoutCategory
      })
    })

    remainingCandidates.filter(candidate =>
      protectedCategories.exists(category => candidate.diversityCategories.contains(category)) ||
      protectedAntiCategories.exists(category => !candidate.diversityCategories.contains(category))
    )
  }

  def excludedCandidates(
                          numRemainingPositions: Int,
                          alreadyElectedCandidates: Set[Candidate],
                          remainingCandidates: Set[Candidate]
                        ): Set[Candidate] = {
    val numCandidatesElectedByCategory: Map[String, Int] = categories.map(category =>
      category -> alreadyElectedCandidates.count(candidate => candidate.diversityCategories.contains(category))
    ).toMap

    // To fill the minimum given the number of spots remaining, further candidates must be part of these categories
    val mandatoryCategories = minimums.keySet.filter(
      category => numCandidatesElectedByCategory(category) + numRemainingPositions <= minimums(category)
    )

    // All the spots for these categories have been filled, so further candidates must not be part of these categories
    val forbiddenCategories = maximums.keySet.filter(
      category => numCandidatesElectedByCategory(category) >= maximums(category)
    )

    remainingCandidates.filterNot(c =>
      mandatoryCategories.forall(c.diversityCategories.contains(_)) &&
        forbiddenCategories.forall(!c.diversityCategories.contains(_))
    )
  }
}

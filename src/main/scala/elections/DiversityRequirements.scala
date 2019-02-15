package elections

object DiversityRequirements {
  val none = new DiversityRequirements(Map.empty, Map.empty)
}

class DiversityRequirements(
                             minimums: Map[String, Int],
                             maximums: Map[String, Int]
                           ) {
  // at least 5 must be racial or national minorities
  // no more than 8 may be men
  val categories = minimums.keySet ++ maximums.keySet

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

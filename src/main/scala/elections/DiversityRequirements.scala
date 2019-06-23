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

  trait Requirement {
    def matches(candidate: Candidate): Boolean
  }
  case class Is(category: String) extends Requirement {
    def matches(candidate: Candidate): Boolean = candidate.diversityCategories.contains(category)
  }
  case class IsNot(category: String) extends Requirement {
    def matches(candidate: Candidate): Boolean = !candidate.diversityCategories.contains(category)
  }

  /*
   example:
    4 seats, max 2 men, min 2 poc.
    elected candidates: 1 white man
    remaining candidates: 1 white man, 1 white woman, 1 poc woman, 1 poc man
    expected outcome: white man excluded, because if elected, not possible to satisfy both constraints
   */

  def doesCandidateMakeSatisfyingRequirementsImpossible(
                                                         numRemainingPositions: Int,
                                                         alreadyElectedCandidates: Set[Candidate],
                                                         remainingCandidates: Set[Candidate],
                                                         candidate: Candidate
                                                       ) = {
    val hypotheticalElectedCandidates = alreadyElectedCandidates + candidate
    val hypotheticalRemainingCandidates = remainingCandidates - candidate
    val hypotheticalNumRemainingPositions = numRemainingPositions - 1

    val numCandidatesElectedByCategory: Map[String, Int] = categories.map(category =>
      category -> hypotheticalElectedCandidates.count(candidate => candidate.diversityCategories.contains(category))
    ).toMap

    val numRemainingToFillForMinimumByCategory = minimums.keys.map(category =>
      category -> (minimums(category) - numCandidatesElectedByCategory(category))
    ).toMap

    val numRemainingToFillForMaximumByCategory = maximums.keys.map(category =>
      category -> (maximums(category) - numCandidatesElectedByCategory(category))
    ).toMap

    val relevantMinimums = minimums.keys.filter(category => numRemainingToFillForMinimumByCategory(category) > 0).toSet

    val relevantMaximums = maximums.keys.filter(category => numRemainingToFillForMaximumByCategory(category) < hypotheticalNumRemainingPositions).toSet

    val minimumMatchesByCategory: Map[String, Set[Candidate]] = relevantMinimums.map(category =>
      category -> hypotheticalRemainingCandidates.filter(_.diversityCategories.contains(category))
    ).toMap

    val maximumMatchesByCategory: Map[String, Set[Candidate]] = relevantMaximums.map(category =>
      category -> hypotheticalRemainingCandidates.filter(!_.diversityCategories.contains(category))
    ).toMap

    if(hypotheticalNumRemainingPositions == 0) {
      // special case for the final seat
      relevantMinimums.exists(!candidate.diversityCategories.contains(_)) ||
      relevantMaximums.exists(candidate.diversityCategories.contains)
    } else {
      // Check if any possible subset of the candidates would satisfy the requirements
      hypotheticalRemainingCandidates.subsets(hypotheticalNumRemainingPositions).forall(candidates => {
        relevantMinimums.exists(category => {
          val numSatisfyingMinimum = candidates.count(_.diversityCategories.contains(category))
          numSatisfyingMinimum < numRemainingToFillForMinimumByCategory(category)
        }) || relevantMaximums.exists(category => {
          val numInMaximumCategory = candidates.count(_.diversityCategories.contains(category))
          numInMaximumCategory > numRemainingToFillForMaximumByCategory(category)
        })
      })
    }
  }

  def excludedCandidates(
                          numRemainingPositions: Int,
                          alreadyElectedCandidates: Set[Candidate],
                          remainingCandidates: Set[Candidate]
                        ): Set[Candidate] = {
    // First, look for any candidates that would make satisfying the requirements impossible
    val tryHardFilteredCandidates = remainingCandidates.filter(
      doesCandidateMakeSatisfyingRequirementsImpossible(
        numRemainingPositions,
        alreadyElectedCandidates,
        remainingCandidates,
        _
      )
    )

    // If that's *all* the candidates, fall back to a solution that will get us a partial result
    if (tryHardFilteredCandidates != remainingCandidates) {
      tryHardFilteredCandidates
    } else {
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
        mandatoryCategories.forall(c.diversityCategories.contains) &&
          forbiddenCategories.forall(!c.diversityCategories.contains(_))
      )
    }

  }
}

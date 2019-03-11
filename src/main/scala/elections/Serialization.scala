package elections

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
  /*
   election:
     type: String/Enum
     candidates: [ {name: String, diversityCategories: [cat1,...] }, ... ]
     diversityRequirements: { minimums: { cat1: Int, ... }, maximums: {cat2: Int} } (defaults to none)
     numPositions: Int(defaults to 1)
     quota: "Droop"|"Hare" (Only for STV, defaults to Droop)
   ballots:
     (Ranked: [candidateName1,...])
     (Score: {candidateId1: value, ...})
   */
object Serialization {
    implicit val candidateReader: Reads[Candidate] = (
      (__ \ "name").read[String] and
      (__ \ "diversityCategories").readWithDefault[Set[String]](Set.empty)
    )((name, diversityCategories) => Candidate(name, diversityCategories))

    implicit val diversityRequirementsReader: Reads[DiversityRequirements] = (
      (__ \ "minimums").readWithDefault[Map[String, Int]](Map.empty) and
      (__ \ "maximums").readWithDefault[Map[String, Int]](Map.empty)
    )((minimums, maximums) => DiversityRequirements(minimums, maximums))

    implicit val stvElectionReader: Reads[SingleTransferableVoteElection] = (
        (__ \ "candidates").read[Set[Candidate]] and
        (__ \ "quota").readWithDefault("droop") and
        (__ \ "numPositions").readWithDefault(1) and
        (__ \ "diversityRequirements").readWithDefault(DiversityRequirements.none)
      )((candidates: Set[Candidate], quotaStr: String, numPositions: Int, diversityRequirements: DiversityRequirements) => {
      val quota = quotaStr.toLowerCase match {
        case "hare" => SingleTransferableVoteElection.HareQuota
        case "droop" => SingleTransferableVoteElection.DroopQuota
        case _ => ???
      }
      new SingleTransferableVoteElection(candidates, numPositions, quota, diversityRequirements)
    })

    implicit val rrvElectionReader: Reads[ReweightedRangeVoteElection] = (
      (__ \ "candidates").read[Set[Candidate]] and
      (__ \ "numPositions").readWithDefault(1) and
      (__ \ "weightConstant").readWithDefault(ReweightedRangeVoteElection.defaultWeightConstant) and
      (__ \ "diversityRequirements").readWithDefault[DiversityRequirements](DiversityRequirements.none)
    )((candidates: Set[Candidate], numPositions: Int, weightConstant: Double, diversityRequirements: DiversityRequirements) => {
      new ReweightedRangeVoteElection(
        candidates = candidates,
        numPositions = numPositions,
        weightConstant = weightConstant,
        diversityRequirements = diversityRequirements
      )
    })

    def readElection(jsonStr: String): Election[_ <: Ballot, _ <: ElectionResult] = readElection(Json.parse(jsonStr))

    def readElection(json: JsValue): Election[_ <: Ballot, _ <: ElectionResult] = {
      (json \ "type").get match {
        case JsString("SingleTransferableVote") => json.validate[SingleTransferableVoteElection].get
        case JsString("ReweightedRangeVote") => json.validate[ReweightedRangeVoteElection].get
      }
    }

    def readElectionAndBallots(jsonStr: String): (Election[_ <: Ballot, _ <: ElectionResult], Seq[Ballot]) = {
      val json = Json.parse(jsonStr)
      val election = readElection((json \ "election").get)
      val ballots = election match {
        case _:SingleTransferableVoteElection => readRankedBallots(election.candidates, (json \ "ballots").get)
        case _:ReweightedRangeVoteElection => readScoreBallots(election.candidates, (json \ "ballots").get)
      }
      (election, ballots)
    }

    def readRankedBallots(candidates: Set[Candidate], value: JsValue): Seq[RankedBallot] = {
      val candidatesByName = candidates.map(c => c.name -> c).toMap
      value match {
        case JsArray(ballots) => ballots.map {
          case JsArray(candidateNames) =>
            val candidateList = candidateNames.map {
              case JsString(name) => candidatesByName(name)
            }
            new RankedBallot(candidateList)
        }
        case _ => ???
      }
    }

    def readScoreBallots(candidates: Set[Candidate], value: JsValue): Seq[ScoreBallot] = {
      val candidatesByName = candidates.map(c => c.name -> c).toMap
      value match {
        case JsArray(ballots) => ballots.map {
          case JsObject(scores) => new ScoreBallot(scores.map {
            case (name: String, JsNumber(score)) => candidatesByName(name) -> score.toInt
          }.toMap)
        }
      }
    }

    implicit val stvRoundResultWriter: Writes[STVRoundResult] = (
      (__ \ "firstPlaceVotes").write[Map[String, Double]] and
      (__ \ "winners").write[Seq[String]] and
      (__ \ "losers").write[Seq[String]] and
      (__ \ "diversityExcluded").write[Seq[String]]
    )(round => {
      val firstPlaceVotes: Map[String, Double] = round.firstPlaceVotes.map { case (c, v) => c.name -> v }
      val winners = round.winners.map(_.name).toSeq
      val losers = round.losers.map(_.name).toSeq
      val diversityExcluded = round.diversityExcluded.map(_.name).toSeq
      (firstPlaceVotes, winners, losers, diversityExcluded)
    })

    implicit val stvElectionResultWriter: Writes[SingleTransferableVoteElectionResult] = (
      (__ \ "winners").write[Seq[String]] and
      (__ \ "rounds").write[Seq[STVRoundResult]]
    )(result => (result.winners.map(_.name), result.rounds))

    implicit val rrvElectionRoundResultWriter: Writes[RRVElectionRoundResult] = (
      (__ \ "winner").write[String] and
      (__ \ "scores").write[Map[String, Double]] and
      (__ \ "diversityExcluded").write[Seq[String]]
    )(round => {
      val winner = round.winner.name
      val scores = round.scores.map { case (c, v) => c.name -> v }
      val diversityExcluded = round.diversityExcluded.map(_.name).toSeq
      (winner, scores, diversityExcluded)
    })

    implicit val rrvElectionResultWriter: Writes[RRVElectionResult] = (
      (__ \ "winners").write[Seq[String]] and
      (__ \ "rounds").write[Seq[RRVElectionRoundResult]]
    )(result => (result.winners.map(_.name), result.rounds))

}

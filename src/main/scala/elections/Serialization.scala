package elections

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.math.BigDecimal.RoundingMode
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
    )((candidates: Set[Candidate], numPositions: Int, weightConstant: BigDecimal, diversityRequirements: DiversityRequirements) => {
      new ReweightedRangeVoteElection(
        candidates = candidates,
        numPositions = numPositions,
        weightConstant = weightConstant,
        diversityRequirements = diversityRequirements
      )
    })

    trait RunElection {
      val election: Election[_, _]
      val ballots: Seq[Ballot]
    }

    case class RunSTVElection(election: SingleTransferableVoteElection, ballots: Seq[RankedBallot]) extends RunElection
    case class RunRRVElection(election: ReweightedRangeVoteElection, ballots: Seq[ScoreBallot]) extends RunElection

    implicit val runElectionFormat: Format[RunElection] = new Format[RunElection] {
      override def writes(o: RunElection): JsValue = ???

      override def reads(json: JsValue): JsResult[RunElection] = {
        val maybeElection = (json \ "election").toOption.map(readElection)
        maybeElection match {
          case Some(election:SingleTransferableVoteElection) => {
            (json \ "ballots").toOption
              .map(ballotsJson => readRankedBallots(election.candidates, ballotsJson))
              .map(ballots => JsSuccess(RunSTVElection(election, ballots)))
              .getOrElse(JsError("ballots"))
          }
          case Some(election:ReweightedRangeVoteElection) => {
            (json \ "ballots").toOption
              .map(ballotsJson => readScoreBallots(election.candidates, ballotsJson))
              .map(ballots => JsSuccess(RunRRVElection(election, ballots)))
              .getOrElse(JsError("ballots"))
          }
          case _ => JsError("election")
        }
      }
    }

    def readElection(jsonStr: String): Election[_ <: Ballot, _ <: ElectionResult] = readElection(Json.parse(jsonStr))

    def readElection(json: JsValue): Election[_ <: Ballot, _ <: ElectionResult] = {
      (json \ "type").get match {
        case JsString("SingleTransferableVote") => {
          json.validate[SingleTransferableVoteElection] match {
            case JsSuccess(value, _) => value
            case JsError(errors) => throw new Exception(errors.toString())
          }
        }
        case JsString("ReweightedRangeVote") => json.validate[ReweightedRangeVoteElection] match {
          case JsSuccess(value, _) => value
          case JsError(errors) => throw new Exception(errors.toString())
        }
        case _ => throw new Exception("election type")
      }
    }

    def readRankedBallots(candidates: Set[Candidate], value: JsValue): Seq[RankedBallot] = {
      val candidatesByName = candidates.map(c => c.name -> c).toMap
      value match {
        case JsArray(ballots) => ballots.map {
          case JsArray(candidateNames) =>
            val candidateList = candidateNames.map {
              case JsString(name) => candidatesByName(name)
              case _ => throw new Exception("malformed ballot entry")
            }
            new RankedBallot(candidateList)
          case _ => throw new Exception("malformed ballot")
        }
        case _ => throw new Exception("malformed ballot list")
      }
    }

    def readScoreBallots(candidates: Set[Candidate], value: JsValue): Seq[ScoreBallot] = {
      val candidatesByName = candidates.map(c => c.name -> c).toMap
      value match {
        case JsArray(ballots) => ballots.map {
          case JsObject(scores) => new ScoreBallot(scores.map {
            case (name: String, JsNumber(score)) => candidatesByName(name) -> score
            case _ => throw new Exception("malformed ballot entry")
          }.toMap)
          case _ => throw new Exception("malformed ballot")
        }
        case _ => throw new Exception("malformed ballot list")
      }
    }

    implicit val stvRoundResultWriter: Writes[STVRoundResult] = (
      (__ \ "firstPlaceVotes").write[Map[String, BigDecimal]] and
      (__ \ "winners").write[Seq[String]] and
      (__ \ "losers").write[Seq[String]] and
      (__ \ "diversityProtected").write[Seq[String]] and
      (__ \ "diversityExcluded").write[Seq[String]] and
      (__ \ "exhaustedBallotWeight").write[BigDecimal]
    )(round => {
      val firstPlaceVotes: Map[String, BigDecimal] = round.firstPlaceVotes.map {
        case (c, v) => c.name -> v.setScale(3, RoundingMode.HALF_UP)
      }
      val winners = round.winners.map(_.name).toSeq
      val losers = round.loser.map(_.name).toSeq
      val diversityProtected = round.diversityProtected.map(_.name).toSeq
      val diversityExcluded = round.diversityExcluded.map(_.name).toSeq
      (firstPlaceVotes, winners, losers, diversityProtected, diversityExcluded,
        round.exhaustedBallotWeight.setScale(3, RoundingMode.HALF_UP))
    })

    implicit val stvElectionResultWriter: Writes[SingleTransferableVoteElectionResult] = (
      (__ \ "winners").write[Seq[String]] and
      (__ \ "quota").write[BigDecimal] and
      (__ \ "rounds").write[Seq[STVRoundResult]]
    )(result => (result.winners.map(_.name), result.quota, result.rounds))

    implicit val rrvElectionRoundResultWriter: Writes[RRVElectionRoundResult] = (
      (__ \ "winner").write[String] and
      (__ \ "scores").write[Map[String, BigDecimal]] and
      (__ \ "diversityExcluded").write[Seq[String]]
    )(round => {
      val winner = round.winner.name
      val scores = round.scores.map { case (c, v) => c.name -> v.setScale(3, RoundingMode.HALF_UP) }
      val diversityExcluded = round.diversityExcluded.map(_.name).toSeq
      (winner, scores, diversityExcluded)
    })

    implicit val rrvElectionResultWriter: Writes[RRVElectionResult] = (
      (__ \ "winners").write[Seq[String]] and
      (__ \ "rounds").write[Seq[RRVElectionRoundResult]]
    )(result => (result.winners.map(_.name), result.rounds))

}

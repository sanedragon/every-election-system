package elections

import Serialization.{RunElection, RunRRVElection, RunSTVElection, readElection}
import play.api.libs.json.Json

class SerializationSpec extends BaseSpec {
  "readElection" should "deserialize a trivial SingleTransferableVoteElection" in {
    val json =
      """{
        |  "candidates": [{"name": "bob"}],
        |  "type": "SingleTransferableVote"
        |}
      """.stripMargin

    val election = readElection(json) match {
      case e: SingleTransferableVoteElection => e
      case x => fail(x.toString)
    }

    election.numPositions shouldBe 1
    election.quota shouldBe SingleTransferableVoteElection.DroopQuota
    election.candidates shouldBe Set(Candidate("bob"))
    election.diversityRequirements shouldBe DiversityRequirements.none
  }

  it should "deserialize a more complicated SingleTransferableVoteElection" in {
    val json =
      """{
        |  "type": "SingleTransferableVote",
        |  "candidates": [
        |                  {"name": "alice", "diversityCategories":["poc"]},
        |                  {"name": "bob", "diversityCategories":["man"]},
        |                  {"name": "cathy", "diversityCategories":[]},
        |                  {"name": "danielle"}
        |                ],
        |  "numPositions": 2,
        |  "quota": "Hare",
        |  "diversityRequirements": {"minimums": {"poc": 1}, "maximums": {"man": 1}}
        |}
      """.stripMargin

    val election = readElection(json) match {
      case e: SingleTransferableVoteElection => e
      case x => fail(x.toString)
    }

    election.numPositions shouldBe 2
    election.quota shouldBe SingleTransferableVoteElection.HareQuota

    election.candidates shouldBe Set(
      Candidate("bob", Set("man")),
      Candidate("alice", Set("poc")),
      Candidate("cathy"),
      Candidate("danielle")
    )

    election.diversityRequirements shouldBe DiversityRequirements(
      maximums = Map("man" -> 1),
      minimums = Map("poc" -> 1)
    )
  }

  it should "deserialize a simple RRV election" in {
    val json =
      """{
        |  "candidates": [{"name": "bob"}],
        |  "type": "ReweightedRangeVote"
        |}
      """.stripMargin

    val election = readElection(json) match {
      case e: ReweightedRangeVoteElection => e
      case x => fail(x.toString)
    }

    election.numPositions shouldBe 1
    election.candidates shouldBe Set(Candidate("bob"))
    election.diversityRequirements shouldBe DiversityRequirements.none
    election.weightConstant shouldBe ReweightedRangeVoteElection.defaultWeightConstant
    election.scoreAggregation shouldBe ReweightedRangeVoteElection.defaultScoreAggregation
  }

  it should "deserialize a more complicated RRV election" in {
    val json =
      """{
        |  "type": "ReweightedRangeVote",
        |  "candidates": [
        |                  {"name": "alice", "diversityCategories":["poc"]},
        |                  {"name": "bob", "diversityCategories":["man"]},
        |                  {"name": "cathy", "diversityCategories":[]},
        |                  {"name": "danielle"}
        |                ],
        |  "numPositions": 2,
        |  "weightConstant": 0.5,
        |  "diversityRequirements": {"minimums": {"poc": 1}, "maximums": {"man": 1}}
        |}
      """.stripMargin

    val election = readElection(json) match {
      case e: ReweightedRangeVoteElection => e
      case x => fail(x.toString)
    }

    election.numPositions shouldBe 2

    election.candidates shouldBe Set(
      Candidate("bob", Set("man")),
      Candidate("alice", Set("poc")),
      Candidate("cathy"),
      Candidate("danielle")
    )

    election.diversityRequirements shouldBe DiversityRequirements(
      maximums = Map("man" -> 1),
      minimums = Map("poc" -> 1)
    )

    election.weightConstant shouldBe 0.5
  }

  it should "deserialize an RRV election and ballots" in {
    val json =
      """{
        |  "election": {
        |    "candidates": [{"name": "bob"}],
        |    "type": "ReweightedRangeVote"
        |  },
        |  "ballots": [
        |    {"bob": 1}
        |  ]
        |}
      """.stripMargin


    val (election, ballots) = Json.parse(json).as[RunElection] match {
      case RunRRVElection(e: ReweightedRangeVoteElection, bs: Seq[ScoreBallot]) => (e, bs)
      case x => fail(x.toString)
    }

    ballots.size shouldBe 1
    ballots.head.scores shouldBe Map(Candidate("bob") -> 1)
  }

  it should "deserialize an STV election and ballots" in {
    val json =
      """{
        |  "election": {
        |    "candidates": [{"name": "bob"}, {"name": "alice"}],
        |    "type": "SingleTransferableVote"
        |  },
        |  "ballots": [
        |    ["bob", "alice"],
        |    ["alice", "bob"]
        |  ]
        |}
      """.stripMargin

    val (election, ballots) = Json.parse(json).as[RunElection] match {
      case RunSTVElection(e: SingleTransferableVoteElection, bs: Seq[RankedBallot]) => (e, bs)
      case x => fail(x.toString)
    }

    ballots.size shouldBe 2
    ballots.head.ranking shouldBe Seq(Candidate("bob"), Candidate("alice"))
    ballots(1).ranking shouldBe Seq(Candidate("alice"), Candidate("bob"))
  }

  "Serialization" should "serialize an STV election result" in {
    val result = new SingleTransferableVoteElectionResult(
      Seq(
        STVRoundResult(
          Map(
            alice -> 5.0,
            bob -> 4.0,
            carol -> 4.0,
            david -> 3.0
          ),
          winners = Set.empty,
          losers = Set(david),
          diversityExcluded = Set.empty,
          exhaustedBallotWeight = 0
        ),
        STVRoundResult(
          Map(
            alice -> 7.0,
            bob -> 4.0,
            carol -> 5.0
          ),
          winners = Set(alice),
          losers = Set.empty,
          diversityExcluded = Set(carol),
          exhaustedBallotWeight = 0
        ),
        STVRoundResult(
          Map(
            bob -> 10.0,
          ),
          winners = Set(bob),
          losers = Set.empty,
          diversityExcluded = Set.empty,
          exhaustedBallotWeight = 0
        ),
      ),
      quota = 7.0
    )

    import Serialization.stvElectionResultWriter

    val resultJson = Json.toJson(result)

    val expected =
      """{
        |  "winners" : [ "Alice", "Bob" ],
        |  "quota" : 7,
        |  "rounds" : [ {
        |    "firstPlaceVotes" : {
        |      "Alice" : 5,
        |      "Bob" : 4,
        |      "Carol" : 4,
        |      "David" : 3
        |    },
        |    "winners" : [ ],
        |    "losers" : [ "David" ],
        |    "diversityExcluded" : [ ],
        |    "exhaustedBallotWeight" : 0
        |  }, {
        |    "firstPlaceVotes" : {
        |      "Alice" : 7,
        |      "Bob" : 4,
        |      "Carol" : 5
        |    },
        |    "winners" : [ "Alice" ],
        |    "losers" : [ ],
        |    "diversityExcluded" : [ "Carol" ],
        |    "exhaustedBallotWeight" : 0
        |  }, {
        |    "firstPlaceVotes" : {
        |      "Bob" : 10
        |    },
        |    "winners" : [ "Bob" ],
        |    "losers" : [ ],
        |    "diversityExcluded" : [ ],
        |    "exhaustedBallotWeight" : 0
        |  } ]
        |}""".stripMargin

    Json.prettyPrint(resultJson) shouldBe expected
  }

  it should "serialize an RRV election result" in {
    val result = new RRVElectionResult(Seq(
      RRVElectionRoundResult(
        Map(
          alice -> 5.0,
          bob -> 4.0,
          carol -> 4.0,
          david -> 3.0
        ),
        diversityExcluded = Set(carol)
      ),
      RRVElectionRoundResult(
        Map(
          bob -> 9.0,
          david -> 4.5
        ),
        diversityExcluded = Set.empty
      ),
    ))

    import Serialization.rrvElectionResultWriter

    val resultJson = Json.toJson(result)

    val expected =
      """{
        |  "winners" : [ "Alice", "Bob" ],
        |  "rounds" : [ {
        |    "winner" : "Alice",
        |    "scores" : {
        |      "Alice" : 5,
        |      "Bob" : 4,
        |      "Carol" : 4,
        |      "David" : 3
        |    },
        |    "diversityExcluded" : [ "Carol" ]
        |  }, {
        |    "winner" : "Bob",
        |    "scores" : {
        |      "Bob" : 9,
        |      "David" : 4.5
        |    },
        |    "diversityExcluded" : [ ]
        |  } ]
        |}""".stripMargin

    Json.prettyPrint(resultJson) shouldBe expected
  }
}

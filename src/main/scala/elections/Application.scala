package elections

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import elections.Serialization._
import de.heikoseeberger.akkahttpplayjson.PlayJsonSupport._

import scala.concurrent.{ExecutionContext, Future}

object Application {
  implicit val actorSystem: ActorSystem = ActorSystem("election-actor-system")
  implicit val actorMaterializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContext = actorSystem.dispatcher

  def main(args: Array[String]): Unit = {
    Http().bindAndHandle(route, "localhost", 8080)
  }

  def route(implicit mat: Materializer) = {
    post {
      path("election") {
        entity(as[RunElection]) {
          case RunSTVElection(election, ballots) =>
            val result = election.countBallots(ballots)
            complete(result)
          case RunRRVElection(election, ballots) =>
            val result = election.countBallots(ballots)
            complete(result)
        }
      }
    }
  }
}

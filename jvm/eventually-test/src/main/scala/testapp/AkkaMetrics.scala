package testapp

import akka.actor.ActorSystem

import java.time.Instant
import scala.concurrent.Future

class AkkaMetrics(implicit system: ActorSystem) {

  def getStartTime(): Future[Instant] = {
    Future.successful(Instant.ofEpochMilli(system.startTime))
  }
}

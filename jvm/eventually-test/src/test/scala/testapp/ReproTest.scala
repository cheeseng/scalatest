package testapp

import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{RestartSource, Sink, Source}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{Assertion, Assertions, BeforeAndAfterAll}
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpecLike

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

class ReproTest
    extends TestKit(ActorSystem("ReproTest"))
    with ImplicitSender
    with AsyncWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with Eventually {

  override def afterAll(): Unit =
    TestKit.shutdownActorSystem(system, 10.seconds, verifySystemShutdown = true)

  val eventually2 = eventually[Future[Assertion]](timeout(scaled(2.seconds)))(_)
  // this one has no leak
  // val eventually2 = akkaEventually[Assertion](_)

  "The akka metrics" should {

    "correctly count" in {
      val metrics = new AkkaMetrics()(system)
      eventually2(
      //akkaEventually(  
        metrics
          .getStartTime()
          .map { time =>
            println("test")
            time.plusSeconds(1).isBefore(Instant.now()) mustEqual true
          }
      )
    }
  }

  def akkaEventually[T](f: => Future[T])(implicit mat: Materializer): Future[T] = {
    val count = new AtomicInteger()
    RestartSource
      .withBackoff(10.millis, 100.millis, 0.2, 10) { () =>
        Source
          .future(f)
          .completionTimeout(100.millis)
          .recoverWithRetries(-1, {
            case e =>
              count.incrementAndGet()
              //println(e.getMessage)
              Source.empty
          })
      }
      .runWith(Sink.headOption)
      .flatMap {
        case None    => Assertions.fail(s"assertion not reached after $count tries")
        case Some(r) => Future.successful(r)
      }(ExecutionContext.global)
  }
}

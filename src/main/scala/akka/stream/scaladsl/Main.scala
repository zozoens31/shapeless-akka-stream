package akka.stream
package scaladsl

import akka.actor.ActorSystem
import scala.concurrent.duration._

/**
  * Created by cyrille on 20/11/2016.
  */
object Main extends App {
  implicit val system = ActorSystem("helloworld")
  implicit val ec = system.dispatcher
  implicit val mat = ActorMaterializer()

  val onEnd = (Source(Stream.from(0)).throttle(1, 500.millis, 1, ThrottleMode.Shaping) to_: Sink.foreach(println)) run ()
  onEnd.onComplete(_ => system.terminate())
}
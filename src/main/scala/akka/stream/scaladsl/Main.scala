package akka.stream
package scaladsl

import Implicits._
import akka.actor.ActorSystem
import shapeless._0

import scala.concurrent.duration._
import scala.util.Random

/**
  * Created by cyrille on 20/11/2016.
  */
object Main extends App {
  implicit val system = ActorSystem("helloworld")
  implicit val ec = system.dispatcher
  implicit val mat = ActorMaterializer()

  val source = Source(Stream.from(0).take(10)).throttle(1, 500.millis, 1, ThrottleMode.Shaping).map(i => (i, i)).async to_: Unzip[Int, Int].async to_:[_0, _0] Sink.foreach[Int](println)
  val (end0, end1) = source.async.to[_0, _0](Flow[Int].map{ i =>
    Thread.sleep(Random.nextInt(1000))
    "second " + i
  }.async).toMat(Sink.foreach(println))(Keep.both) run ()
  (end0 zip end1).onComplete(_ => system.terminate())
}
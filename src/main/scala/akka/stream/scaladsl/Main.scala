package akka.stream
package scaladsl

import Implicits._
import akka.actor.ActorSystem
import shapeless.{Nats, _0}

/**
  * Created by cyrille on 20/11/2016.
  */
object Main extends App with Nats {
  implicit val system = ActorSystem("helloworld")
  implicit val ec = system.dispatcher
  implicit val mat = ActorMaterializer()

  val source = (Source.single(0) to_:[_0, _0] MergeNat[Int, _2]() to_: Flow[Int].map(i => (i, i)) to_: Unzip[Int, Int].async to_:[_0, _0] Flow[Int].map(_ + 1)).loop[_0, _0]
  val end = source.async runWith Sink.foreach(println)
  end.onComplete(_ => system.terminate())
}


package akka.stream
package scaladsl

import Implicits._
import akka.actor.ActorSystem
import shapeless.{Nats, _0}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * Created by cyrille on 20/11/2016.
  */
object Main extends App with Nats {
  implicit val system = ActorSystem("helloworld")
  implicit val ec = system.dispatcher
  implicit val mat = ActorMaterializer()

  type N = BigDecimal
  val start0 = Source.single[N](0) to_:[_0, _0] MergeNat[N, _2]()
  val start1 = Source[N](List(0, 1)) to_:[_0, _0] MergeNat[N, _2]()
  val source = (start0 to_:[_0, _0] start1 to_:[_0, _1] ZipWith((_: N) + (_: N)) to_: BroadcastNat[N, _3]().async).loop[_0, _0].loop[_0, _0]
  val end = source to Flow[N].take(100) runWith Sink.foreach(println)
  val finish = end.flatMap(_ => system.terminate())
  Await.result(finish, Duration.Inf)
}

